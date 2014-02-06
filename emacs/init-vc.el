;;; init-vc.el --- VC customizations

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1995
;; Public domain

;; $Id: init-vc.el,v 1.12 2011/12/05 21:52:27 friedman Exp $

;;; Commentary:
;;; Code:


;;;;;;
;;; Defuns
;;;;;;

;; ----------
;; Makes vc-checking have same heuristic as add-change-log-entry does to
;; guess what function you are in and insert it into the change log for
;; you.  From Roland McGrath <roland@prep.ai.mit.edu>.
(defadvice vc-checkin (around sinit:guess-defun compile activate)
  "Insert a guessed defun name from `add-log-current-defun'."
  (let ((defun-guess (and (not (ad-get-arg 2))
                          (equal (abbreviate-file-name
                                  (file-truename
                                   (if (consp (ad-get-arg 0))
                                       (car (ad-get-arg 0))
                                     (ad-get-arg 0))))
                                 buffer-file-truename)
                          (add-log-current-defun))))
    (prog1 ad-do-it
      (if defun-guess
          (progn
            (goto-char (point-max))
            (insert "(" defun-guess "): "))))))

;; ----------
;; Insert the file's log new entries in ChangeLog.
;; From Roland McGrath <roland@prep.ai.mit.edu>
;; Changed by Noah to see if log actually exists, since I don't keep change
;; logs in some directories.
(defun ivc-checkin-add-log ()
  (save-match-data
    (cond ((string-match "\.texi\(nfo\)?$" buffer-file-name))
          ((string-match "/ChangeLog$" buffer-file-name))
          (t
           (let ((log (find-change-log)))
             (and log
                  (file-exists-p log)
                  (y-or-n-p (format "Update %s for %s changes? "
                                    (file-relative-name log)
                                    (file-relative-name buffer-file-name)))
                  (progn
                    (save-excursion (set-buffer (find-file-noselect log))
                                    (setq buffer-read-only nil))
                    (vc-comment-to-change-log nil log))))))))

;; ----------
(defun ivc-ftp-file-name-p (&optional file-name)
  (let ((match-data (match-data)))
    (prog1
        (and (string-match "^/\\([^@:/]*@\\)?[^@:/]*:.*"
                           (or file-name (buffer-file-name) ""))
             t)
      (store-match-data (match-data)))))

;; ----------
;; I don't want CVS-controlled buffers to be modifiable by accident; this
;; will force me to "check out" unmodified files by toggling the read-only
;; state of the buffer first.
;; If visiting a ChangeLog with add-change-log-entry, don't toggle read-only.
;;
;; I haven't tested to see if this works in any emacs prior to 19.30, but
;; it goes on find-file-hooks.
(defun ivc-make-unmodified-cvs-read-only ()
  (cond ((or (eq major-mode 'change-log-mode)
              (memq this-command '(add-change-log-entry
                                   add-change-log-entry-other-window))))
        ((not (and (fboundp 'vc-backend)
                   (eq 'CVS (vc-backend buffer-file-name)))))
        ((fboundp 'vc-state) ; emacs 21
         (let ((state (vc-state buffer-file-name)))
           (if (not (memq state '(edited needs-merge)))
               (setq buffer-read-only t)))
         (force-mode-line-update))
        (t
         (if (or (null (vc-locking-user buffer-file-name))
                 ;; If the file is locked by some other user, make the
                 ;; buffer read-only.  Like this, even root cannot modify a
                 ;; file without locking it first.
                 (not (string= (user-login-name)
                               (vc-locking-user buffer-file-name))))
             (setq buffer-read-only t))
         (force-mode-line-update))))

;; ----------
;; Based on idea by "thi" <ttn@mingle.glug.org>, but modified.
(defadvice vc-print-log (after sinit:pretty-print first activate)
  "Make output more readable."
  (let* ((fn (file-name-nondirectory (buffer-file-name vc-parent-buffer)))
         (sep (concat "#" (make-string (- (min 80 (frame-width)) 4) ?-) "\n"))
         (rep (format "\n\n%s# %s \\1\t\\2\t\\3\t\\5\n%s" sep fn sep))
         (re  (concat "^-+\nrevision \\(\\S-*\\).*\n"
                      "date: \\([^;]+\\);\\s-+"
                      "author: \\([^;]+\\);"
                      "\\(\\s-+.*\\s-+"
                      "lines: \\(.*\\)\\)*")))
    (goto-char (point-max))
    (while (re-search-backward re (point-min) t)
      (replace-match rep))
    (goto-char (point-min))))


;;;;;;
;;; Mode hooks
;;;;;;

(add-hook 'vc-checkin-hook 'ivc-checkin-add-log)
(add-hook 'vc-checkin-hook 'ivc-make-unmodified-cvs-read-only 'append)
(add-hook 'find-file-hooks 'ivc-make-unmodified-cvs-read-only 'append)


;;;;;;
;;; Variables
;;;;;;

;; Just because there are version control files doesn't mean I want to lose
;; transient work between checkins.
(setq-default vc-make-backup-files t)

(setq vc-command-messages          t)
(setq vc-consult-headers           t)
(setq vc-display-status            t)
(setq vc-follow-symlinks           'ask)
(setq vc-keep-workfiles            t)
(setq vc-maximum-comment-ring-size 128)
(setq vc-mistrust-permissions      t)
(setq vc-suppress-confirm          nil)

(if (and (eq (emacs-variant) 'emacs)
         (>= (emacs-version-major) 21))
    (setq vc-rcs-checkout-switches '("-M"))
  (setq vc-checkout-switches '("-M")))

(provide 'init-vc)

;;; init-vc.el ends here.
