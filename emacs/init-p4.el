;;; init-p4.el --- customizations for p4.el

;; Copyright (C) 2006, 2010 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-p4.el,v 1.5 2010/01/22 19:59:05 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'dbfrobs)

;;;;;;
;;; Defvars
;;;;;;

;; Not sure if these rules are optimal from a scanning perspective
(defconst ip4-spec-font-lock-keywords
  '(("^\\([A-Za-z0-9_]+:\\)\\([ ]+\\)?"
     (1 font-lock-variable-name-face)
     (2 'trailing-whitespace nil t))

    ("^ +\\|\\>[\t ]+$"
     (0 'trailing-whitespace))

    ("^\t\\([\t ]+\\)$"
     (1 'trailing-whitespace))

    ("[ \t]+\\([+-]\\)?\\(//[^/\n]+\\)"
     (1 'font-lock-doc-face nil t)
     (2 'font-lock-preprocessor-face))

    ("^#.*"
     (0 font-lock-comment-face t))
    ))


;;;;;;
;;; Defuns
;;;;;;

(defun ip4-fetch-field (field)
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let* ((case-fold-search t)
               (re (format "^%s:\t*\n?" (regexp-quote field)))
               (buf (current-buffer))
               beg end)
          (when (re-search-forward re nil t)
            (setq beg (point))
            (re-search-forward "^$" nil t)
            (setq end (point))
            (with-temp-buffer
              (insert-buffer-substring-no-properties buf beg end)
              (goto-char (point-min))
              (while (re-search-forward "^\t" nil t)
                (replace-match ""))
              (when (re-search-forward "\n+\\'" nil t)
                (replace-match ""))
              (buffer-string))))))))

(defun ip4-font-lock-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((ip4-spec-font-lock-keywords)
                             nil nil ((?\_ . "w"))))
  (font-lock-mode 1))

(defun ip4-fix-whitespace ()
  (interactive)
  (save-excursion
    (mapc (lambda (pair)
            (let ((re (car pair))
                  (rm (car (cdr pair))))
              (goto-char (point-min))
              (while (re-search-forward re nil t)
                (replace-match rm))))
      `((,(format "^%s" (make-string tab-width ?\x20)) "\t")
        ("[ \t]+$" "")
        ("^ +"     "\t")
        ("\n+\\'" "\n")))))

(defun ip4-spec-fmtcols-view ()
  (interactive)
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward "^\\(files\\|view\\|reviews\\|typemap\\):" nil t)
          (let ((field (downcase (match-string 1)))
                beg end)
            (forward-line 1)
            (beginning-of-line)
            (setq beg (point))
            (re-search-forward "^$" nil 'move-to-end)
            (setq end (point))
            (narrow-to-region beg end)

            (cond ((member field '("view" "typemap"))
                   (shell-command-on-region (point-min) (point-max)
                      "fmtcols -n3 -S '\t'"
                      (current-buffer) t))

                  ((string= field "files")
                   (shell-command-on-region (point-min) (point-max)
                      "fmtcols -n3 -S ' # ' -s '\\s+#\\s+'"
                      (current-buffer) t)))

            (goto-char (point-min))
            (cond ((re-search-forward "^\t[-+]//" nil t)
                   (goto-char (point-min))
                   (while (re-search-forward "^\t//" nil t)
                     (replace-match "\t //")
                     (if (re-search-forward " " (line-end-position) t)
                         (delete-char -1)))))))))))

;; old name
(defalias 'indent-p4-view-columns 'ip4-spec-fmtcols-view)

(defun ip4-spec-relocate-comments ()
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^#")
      (re-search-forward "^\n+")
      (let ((text (buffer-substring (point-min) (match-end 0))))
        (delete-region (point-min) (match-end 0))
        (goto-char (point-max))
        (insert "\n" text)
        (goto-char (point-min))
        (when (re-search-forward "\n\\(\n+\\)\\'" nil t)
          (delete-region (match-beginning 1) (point-max)))))))

(defun ip4-mode-setup ()
  (interactive)
  (ip4-spec-fmtcols-view)
  (ip4-spec-relocate-comments)
  (ip4-font-lock-setup)

  ;; Make sure form is autosaved, to guard against accidental lossage.
  (unless buffer-file-name
    (make-local-variable 'backup-inhibited)
    (setq backup-inhibited t)
    (setq buffer-file-name (make-temp-file "p4-"))
    (auto-save-mode 1)))

(defun ip4-pop-window-config (&optional num)
  (unless num (setq num 1))
  (p4-pop-window-config num))


;;;;;;
;;; Advice
;;;;;;

;; The window configuration stack maintained by p4.el just seems to
;; accumulate cruft because I tend to kill buffers rather than "exit" them.
;; In any case I use escreen to maintain window configs; p4.el's behavior
;; is surprising when one tends to switch between windows frequently while
;; editing specs or changelists anyway.

(defadvice p4-push-window-config (around sinit:no-op activate)
  "This function has been disabled; it does nothing."
  nil)

(defadvice p4-pop-window-config (around sinit:no-op activate)
  "This function has been disabled; it does nothing."
  nil)

(defadvice display-buffer (around sinit:p4hack activate)
  "Don't display p4-output-buffer-name."
  (unless (eq (get-buffer (ad-get-arg 0))
              (get-buffer p4-output-buffer-name))
    ad-do-it))

;; my god, what have I done
(defadvice switch-to-buffer-other-window (around sinit:p4hack activate)
  "Prevent p4.el commands from ignoring my same-window-regexps."
  (if (dbfrobs:in-call-stack-p "^p4-")
      (display-buffer (ad-get-arg 0))
    ad-do-it))

(defadvice p4-async-call-process (around sinit:backing-store activate)
  "Auto-save p4 forms buffers until successful completion of commands."
  (let ((orig-buffer (current-buffer))
        (backing-store-file (buffer-file-name)))
    (when backing-store-file
      (ip4-fix-whitespace)
      (basic-save-buffer)
      (setq buffer-file-name nil))
    (unwind-protect
        (progn ad-do-it)
      (message "%s" (save-excursion
                      (set-buffer p4-output-buffer-name)
                      (buffer-substring (point-min) (1- (point-max)))))
      (cond ((not backing-store-file))
            ((buffer-live-p orig-buffer)
             (setq buffer-file-name backing-store-file))
            (t
             (delete-file backing-store-file))))))


;;;;;;
;;; Mode hooks
;;;;;;

(add-hook 'p4-async-command-hook 'ip4-mode-setup)


;;;;;;
;;; Variables
;;;;;;

(add-to-list 'same-window-regexps "^\\*P4 ")


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;


(provide 'init-p4)

;;; init-p4.el ends here.
