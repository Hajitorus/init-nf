;;; init-dired.el --- dired startup

;; Copyright (C) 1991, 92, 93, 94, 95, 96, 97, 98, 99, 00, 04, 2005 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-dired.el,v 1.19 2012/01/04 23:03:34 friedman Exp $

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

(require 'emacs-variants)
(require 'string-fns)


;;;;;;
;;; Variable declarations
;;;;;;

;; ----------
(defvar idired-font-lock-keywords
  (list
   ;; Directory headers.
   (list dired-subdir-regexp '(1 font-lock-type-face))
   ;; Dired marks.
   (list dired-re-mark
         '(0 font-lock-constant-face)
         '(".+" (dired-move-to-filename) nil (0 font-lock-warning-face)))
   ;; If file is group or other writable, colorize the write flags
   (list (concat dired-re-maybe-mark dired-re-inode-size
                 "[-bcdps]....\\(w\\)") '(1 font-lock-comment-face))
   (list (concat dired-re-maybe-mark dired-re-inode-size
                 "[-bcdps].......\\(w\\).") '(1 font-lock-comment-face))
   ;; Executable files
   (list dired-re-exe '(".+" (dired-move-to-filename) nil
                        (0 font-lock-variable-name-face)))
   ;; Subdirectories.
   (list dired-re-dir '(".+" (dired-move-to-filename) nil
                        (0 font-lock-function-name-face)))
   ;; Symbolic links.
   (list dired-re-sym '(".+" (dired-move-to-filename) nil
                        (0 font-lock-keyword-face)))
   ;; Special files
   (list (concat dired-re-maybe-mark dired-re-inode-size "[bcps]")
         '(".+" (dired-move-to-filename) nil (0 font-lock-constant-face)))
   ;; Files suffixed with `completion-ignored-extensions'.
   '(eval .
     (let ((exts (mapcar 'regexp-quote completion-ignored-extensions)))
       ;; It is quicker to first find just an extension, then go back
       ;; to the start of that file name.  So we do this complex
       ;; MATCH-ANCHORED form.
       (list (concat "\\(" (mapconcat 'identity exts "\\|") "\\|#\\)$")
             '(".+" (dired-move-to-filename) nil
               (0 font-lock-string-face)))))))

;; ----------
(defvar idired-long-listing-min-window-width 80
  "*Minimum window width for dired to use normal listing switches.
Otherwise, dired will use `idired-short-listing-switches' in place of
`listing-switches'.")

;; ----------
(defvar idired-gnu-ls-version
  (let ((s (with-command-output-to-string "ls" "--version")))
    (save-match-data
      (if (string-match "^ls\\s-+\([^\)\n]*\)\\s-+\\([0-9.]+[a-z]*\\)" s)
          (match-string 1 s)
        "0"))))

;; ----------
(defvar idired-short-listing-switches
  (if (string-lessp "3.16g" idired-gnu-ls-version)
      "-lav"
    "-la")
  "*Listing switches for narrow emacs windows.
Narrow is defined in terms of being less than
`idired-long-listing-min-window-width' columns wide.")

;; ----------
(defvar idired-all-confirmation-types
  '(byte-compile chgrp chmod chown compress copy delete hardlink
    load move print shell symlink uncompress recursive-delete
    kill-file-buffer kill-dired-buffer patch create-top-dir
    revert-subdirs))

;; ----------
(defvar idired-confirm-types
  '(delete recursive-delete kill-file-buffer))


;;;;;;
;;; Defuns
;;;;;;

;; ----------
(defun idired-mode-setup ()
  (idired-set-listing-switches)
  (idired-set-mode-line)

  (make-local-variable 'dired-omit-files-p)
  (setq dired-omit-files-p
        (<= 4 (prefix-numeric-value current-prefix-arg)))

  (when (eq (emacs-variant) 'emacs)
    (make-local-variable 'font-lock-keywords)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-keywords idired-font-lock-keywords
          font-lock-defaults '(idired-font-lock-keywords t)))

  (setq truncate-lines t)
  (setq case-fold-search t))

;; ----------
(defun idired-no-confirm ()
  (let ((no-confirm (copy-sequence idired-all-confirmation-types))
        (confirm idired-confirm-types))
    (while confirm
      (setq no-confirm (delq (car confirm) no-confirm))
      (setq confirm (cdr confirm)))
    no-confirm))

;; ----------
(defun idired-set-listing-switches ()
  (cond ((<= (window-width) idired-long-listing-min-window-width)
         (setq dired-actual-switches
               (if (fboundp 'dired-make-switches-list)
                   (dired-make-switches-list idired-short-listing-switches)
                 idired-short-listing-switches))
         (setq dired-internal-switches dired-actual-switches))))

;; ----------
(defun idired-set-mode-line ()
  (setq mode-line-modified (default-value 'mode-line-modified))
  (setq mode-line-buffer-identification
        (default-value 'mode-line-buffer-identification))
  (and (fboundp 'dired-sort-set-modeline)
       (dired-sort-set-modeline)))


;;;;;;
;;; Advice
;;;;;;

;; 2011-12-30 Not needed for native Emacs dired, but kept on the off chance
;; that the XEmacs dired is in use with Emacs.
;;
;; The following pertain to development sources in CVS as of 2004-06-14;
;; In older released versions, the load mechanics were slightly different.
;; However, this implementation is compatible with them.
;;
;; Unless dired-handler-fn is removed from file-name-handler-alist,
;; the call stack (from outer to inner) looks something like this:
;;
;;   Fload (file)
;;     openp (file)
;;       Ffind_file_name_handler (file, ...)
;;       => fd == -2, found=absolute-path ; file!=found
;;     Ffind_file_name_handler(found, t) => dired-handler-fn
;;       (dired-handler-fn 'load file)  ; note file<-absolute path
;;         => inhibits op=load, calls op
;;         Fload (file)
;;           openp (file)
;;             Ffind_file_name_handler (file, ...) => dired-handler-fn
;;               => fd == -2, found=file  ; note file==found this time
;;           Ffind_file_name_handler (found, 'load) => nil
;;           fdopen (-2) => NULL
;;           [load fails, "failure to create stdio stream for foo"]
;;
;; Instead, we want the call to go like this:
;;
;;   Fload (file)
;;     openp (file)
;;       Ffind_file_name_handler (file, 'file-exists-p) => dired-handler-fn
;;       => fd == -2, found=absolute-path
;;     Ffind_file_name_handler(found, 'load) => dired-handler-fn
;;       (dired-handler-fn 'load file) ; file<-absolute path
;;         => file-name-handler-alist <- delete dired-handler-fn
;;            inhibits op=load, calls op
;;         Fload (file)
;;           openp (file)
;;             Ffind_file_name_handler (file, ...) => nil
;;             => fd == n
;;           fdopen (n) = stream
;;           [loading commences]
;;
;; To avoid the former call stack mess, just remove dired-handler-fn from
;; after-load-alist while handling the op.
(defadvice dired-handler-fn (around sinit:load-patch activate)
  "The following comments apply to Emacs 20 and above, when used with the
non-bundled Dired package (the same as the one bundled with XEmacs).

This handler is only active when there is at least one dired buffer; when
the last dired buffer is killed, this handler is removed from
`file-name-handler-alist'.  This handler claims to handle every file when
it is active.

The primitive function `Fload' calls `openp' (see lread.c) to look up the
file name in `load-path'.  Once `openp' finds the full name, it makes
another call to `Ffind_file_name_handler' to decide whether the file is
\"remote\" or not; if a handler is found, the file is considered remote and
the function returns the special value 0 (n.b. in Emacs 21 it returns -2).
When `Fload' sees this value, it looks up the file name handler on the full
file name; in most cases that will ultimately bottom out in another call to
`Fload' with the full file name as the file to load because the file is
actually local.

As a consequence, forms on `after-load-alist' for entries that match the
original file name argument to `load' may never get evaluated.  So here we
use `eval-after-load-forms-for' (see load-fns.el), which performs a broader
search for forms to evaluate."
  (cond ((eq (ad-get-arg 0) 'load)
         ;; Don't let Fload evaluate after-load forms; we'll do it
         ;; ourselves.
         (let ((after-load-alist nil)
               ;; See lengthy comments above.
               (file-name-handler-alist
                (delq (rassq 'dired-handler-fn
                             (copy-sequence file-name-handler-alist))
                      file-name-handler-alist)))
           ad-do-it)
         (when ad-return-value
           (eval-after-load-forms-for (ad-get-arg 1))))
        (t
         ad-do-it)))

;; Only needed for XEmacs' dired.
(when (fboundp 'dired-sort-type)
  (defadvice dired-sort-toggle-or-edit (before sinit:ls-switch-hack activate)
    "Handle `-v' and `-t' interoperation when toggling.

When toggling to sort by date, make sure `-v' flag is not part of the
listing switches, since that will override the prepended `-t' flag which
specifies sorting by date.

Conversely, when switching back to sort by name, prepend the `-v' flag if
it is missing from the current switch set but appears in
`dired-listing-switches' (which see)."

    (let* ((cell (nthcdr 3 (assoc (dired-current-directory) dired-subdir-alist)))
           (switches (car cell)))
      (cond ((not (null (ad-get-arg 0))))
            ((and (eq (dired-sort-type switches) 'name)
                  (member ?v switches))
             (setcar cell (delete ?v switches)))
            ((and (eq (dired-sort-type switches) 'date)
                  (not (member ?v switches))
                  (save-match-data (string-match "v" dired-listing-switches)))
             (setcar cell (cons ?v switches)))))))

;; Emulation of xemacs function.  Needed by dired-shell.
(defun replace-in-string (str regexp newtext &optional literal)
  (replace-regexp-in-string regexp newtext str nil literal))


;;;;;;
;;; Hook initialization
;;;;;;

;; ----------
;; Need this because dired has its own revert buffer function.
(add-hook 'dired-after-readin-hook 'idired-mode-setup)

;; ----------
(add-hook 'dired-mode-hook 'idired-mode-setup)

;;;;;;
;;; Variable settings
;;;;;;

(setq dired-copy-preserve-time           t
      dired-backup-if-overwrite          t
      dired-chown-program                "chown"
      dired-gnutar-program               "tar"
      dired-guess-have-gnutar            dired-gnutar-program
      dired-ls-program                   "ls"
      dired-ls-F-marks-symlinks          t
      dired-no-inline-headerlines        nil
      dired-listing-switches             (concat idired-short-listing-switches "s")
      dired-show-ls-switches             t
      dired-compression-method           'gzip
      dired-no-confirm                   (idired-no-confirm))

(setq dired-recursive-deletes            'top)

(define-key dired-mode-map "\M-=" 'dired-backup-diff)

(provide 'init-dired)

;;; init-dired.el ends here.
