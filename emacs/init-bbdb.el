;;; init-bbdb.el --- initialization for the Insidious Big Brother Database

;; Copyright (C) 1999, 2000, 2005 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-bbdb.el,v 1.13 2011/01/20 23:58:46 friedman Exp $

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

;; M-x bbdb-insinuate-noah

;;; Code:

(require      'init-common)
(require-soft 'bbdb-autoloads)
(require      'bbdb-hooks)
(require      'win-disp-util)
(require-soft 'fff-elisp)
(require-soft 'timer)

(defvar ibbdb-bogus-nets nil)

(defvar ibbdb-buffer-name " *bbdb data*")
(defvar ibbdb-save-timer nil)


;;; mode-line format vars

(defvar ibbdb-display-readonly-mode-line-string " %%BBDB")

(defvar ibbdb-readonly-mode-line-format
  ;; In Emacs 21, only show that BBDB is readonly in mail-related modes
  (cond ((and (eq (emacs-variant) 'emacs)
              (>= (emacs-version-major) 21))
         '(:eval (ibbdb-display-readonly-minor-mode)))
        (t 'ibbdb-display-readonly-mode-line-string)))

;; In emacs 21, only show readonly status for bbdb in these major modes
(defvar ibbdb-display-readonly-minor-modes
  '(bbdb-mode
    mail-mode
    vm-mode
    vm-presentation-mode
    vm-summary-mode))


;; ----------
(defun ibbdb-complete-name (&optional start-pos)
  "Like `bbdb-complete-name', but don't pop up buffer if not already displayed.
If bbdb buffer is not already visible, then do not try to update it, since
that would create a new window."
  (interactive)
  (let ((bbdb-completion-display-record bbdb-completion-display-record))
    (or (wdu-buffer-window bbdb-buffer-name)
        (setq bbdb-completion-display-record nil))
    (bbdb-complete-name start-pos)))

;; ----------
(defun ibbdb-delete-bogus-nets (record)
  "Deletes bogus addresses from user records.
Bogus addresses are listed in `ibbdb-bogus-nets'."
  (when ibbdb-bogus-nets
    (let* ((nets (bbdb-record-net record))
           (bogus ibbdb-bogus-nets)
           (changep nil))
      (while bogus
        (unless changep
          (setq changep (member (car bogus) nets)))
        (setq nets (delete (car bogus) nets))
        (setq bogus (cdr bogus)))
      (when changep
        (bbdb-record-set-net record nets)))))

;; ----------
;; This is defined as a separate defun so it can be compiled for speed.
;; It will be called for every modeline update!
(defun ibbdb-display-readonly-minor-mode ()
  (if (memq major-mode ibbdb-display-readonly-minor-modes)
      ibbdb-display-readonly-mode-line-string
    ""))

;; ----------
(defadvice bbdb-display-records-1 (around sinit:no-help-mode activate)
  "Temporarily rebind `temp-buffer-setup-hook' without `help-mode-setup'."
  (let ((temp-buffer-setup-hook
         (and (boundp 'temp-buffer-setup-hook)
              (delq 'help-mode-setup (copy-sequence temp-buffer-setup-hook)))))
    ad-do-it))

;; ----------
(defun ibbdb-db-buffer-initialize ()
  (rename-buffer ibbdb-buffer-name)
  (setq nuke-trailing-whitespace-p nil)
  (and (require-soft 'protbuf)
       (protect-buffer-from-kill-mode 1))
  ;; No numbered backups by default for the database.
  (make-local-variable 'version-control)
  (setq version-control nil))

;; ----------
(defadvice bbdb-get-addresses (around ibbdb-delete-bogus-nets activate)
  "Prevent returning addresses in `ibbdb-bogus-nets'.
This is particularly useful when scanning mail messages because a real-name
part might match another *existing* database record, which results in being
asked whether the bogus address should be added to that record."
  (when ibbdb-bogus-nets
    (let ((uninteresting (ad-get-arg 1))
          (bogus (concat "\\<" (regexp-opt ibbdb-bogus-nets) "\\>")))
      (setq uninteresting
            (if uninteresting
                (format "\\(?:%s\\|%s\\)" uninteresting bogus)
              bogus))
      (ad-set-arg 1 uninteresting)))
  (let ((bbdb-user-mail-names nil))
    ad-do-it))

;; ----------
(defun ibbdb-set-save-interval (secs)
  (cond ((timerp ibbdb-save-timer)
         (cancel-timer ibbdb-save-timer)
         (setq ibbdb-save-timer nil)))
  (and secs
       (setq ibbdb-save-timer (run-with-timer secs secs 'bbdb-save-db))))

;; ----------
(defun ibbdb-toggle-readonly (&optional prefix)
  (interactive "P")
  (set-minor-mode-string 'bbdb-readonly-p ibbdb-readonly-mode-line-format t)
  (bbdb-save-db t t)
  (cond
   ((null prefix)
    (setq bbdb-readonly-p (not bbdb-readonly-p)))
   ((>= prefix 0)
    (setq bbdb-readonly-p t))
   (t
    (setq bbdb-readonly-p nil))))


(add-hook 'bbdb-after-read-db-hook 'ibbdb-db-buffer-initialize)

(add-hook 'bbdb-change-hook 'ibbdb-delete-bogus-nets)
(add-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
(add-hook 'bbdb-change-hook 'bbdb-delete-redundant-nets)

(add-hook 'bbdb-create-hook 'bbdb-creation-date-hook)
;(add-hook 'bbdb-create-hook 'bbdb-whois)

(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

(ibbdb-set-save-interval 300)


(setq bbdb-file                           "~/lib/misc/bbdb")
(setq bbdb-electric-p                     t)
(setq bbdb-case-fold-search               t)
(setq bbdb-readonly-p                     nil)
(setq bbdb-auto-revert-p                  nil)
(setq bbdb-notice-auto-save-file          t)
(setq bbdb-message-caching-enabled        t)
(setq bbdb-offer-save                     t)

(setq bbdb-complete-name-allow-cycling    t)
(setq bbdb-completion-type                nil) ; 'primary-or-name
(setq bbdb-completion-display-record      t)

(setq bbdb-always-add-addresses           'ask)
(setq bbdb-new-nets-always-primary        'never) ; never means append
(setq bbdb-canonicalize-redundant-nets-p  t)
(setq bbdb-expand-mail-aliases            t)

(setq bbdb-default-area-code              510)
(setq bbdb-default-country                "")
(setq bbdb-north-american-phone-numbers-p t)

;(setq bbdb-user-mail-names               nil)

(setq bbdb-use-pop-up                     nil) ; t horiz
(setq bbdb-pop-up-target-lines            5)

(setq bbdb/mail-auto-create-p             t)
(setq bbdb/news-auto-create-p             nil)

(setq bbdb-quiet-about-name-mismatches    nil)
(setq bbdb-use-alternate-names            t)

(define-key bbdb-mode-map "w" 'bbdb-whois)

;; Seek out the info file for the version of bbdb we're using, which may
;; not be installed in a default location.
(when (and (null bbdb-info-file)
           (featurep 'fff-elisp))
  (let ((loaded (fff-elisp-load-history-file-name
                 (fff-elisp-load-history-elt-by 'feature 'bbdb)))
        (info nil))
    (when (save-match-data (string-match "\\.elc\\'" loaded))
      (setq loaded (fff-emacs-lisp-bytecode-source-file-name loaded)))
    (setq info (expand-file-name "../texinfo/bbdb.info" (file-name-directory loaded)))
    (when (file-exists-p info)
      (setq bbdb-info-file info))))


(add-forms-to-after-load-alist
  '(("listbuf"
     (add-list-members 'listbuf-ignore-buffer-name-regexp-list
       (concat "^" (regexp-quote ibbdb-buffer-name) "$")))))

(provide 'init-bbdb)

;;; init-bbdb.el ends here.
