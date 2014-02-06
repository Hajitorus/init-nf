;;; init-xemacs-19.el --- startup for XEmacs 19.12 or later.

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Created: 1994-12-28
;; Public domain.

;; $Id: init-xemacs-19.el,v 1.10 2011/12/05 21:52:27 friedman Exp $

;;; Commentary:

;; This assumes XEmacs 19.12 or later, since it does not alias screen/frame
;; terminology etc. missing from earlier Lucid/XEmacs implementations.

;;; Code:

;;;;;;
;;; Defuns
;;;;;;

(defun mouse-move-browse-url-at-point ()
  "Move point to position mouse is over, then call browse-url-at-point."
  (interactive)
  (call-interactively 'mouse-track)
  (browse-url-at-point))

(defadvice disabled-command-hook (around sinit:broken-disabled-command-hook activate)
  "Work around egregious lossage in XEmacs 19.14 through 20.4 (at least).

The variable `disabled-command-hook' is nonfunctional unless set to the
symbol `disabled-command-hook'; any other value is not only ignored, but
the disabled command won't execute.  Thus, the only way to alter disabled
command behavior is to change the function definition of this symbol."
  (apply 'enable-and-run-command (ad-get-args 0)))


;; The error handler in replace-auto-mode (my own function) fails to work
;; in XEmacs prior to 19.13.  That's because the semantics of
;; debug-on-error were changed to override condition-case.  In XEmacs 19.13
;; this was finally changed back and a new variable for the original Lemacs
;; semantics was created.
(setq auto-mode-alist (copy-alist auto-mode-alist))

(load-offer-compile "init-common")


;;;;;;
;;; Variables
;;;;;;

(or t ; disable for now
    (featurep 'hack-locals)
    (setq enable-local-variables 'query
          enable-local-eval      'query))

(setq auto-save-timeout               15
      next-line-add-newlines          nil
      undo-threshold                  megabyte
      undo-high-threshold             moby-bignum)

;; efs.el
(setq efs-binary-file-name-regexp     ".*"
      efs-debug-ftp-connection         t
      efs-disable-netrc-security-check t
      efs-expire-ftp-buffers           nil
      efs-generate-anonymous-password  t
      efs-make-backup-files            t
      efs-disable-netrc-security-check t
      efs-maximize-idle                t)

;; Override setting in init-common.el; see defadvice for this symbol above.
(setq disabled-command-hook 'disabled-command-hook)


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

;(global-set-key [(control button2)] 'mouse-move-browse-url-at-point)

(setq frame-title-format (symbol-name (emacs-variant)))

;; 1st arg eq t => use raw mode (nil means cbreak)
;; 2nd arg eq t => use C-s/C-q flow control (only matters in cbreak mode)
;; 3rd arg eq t => use meta bit.
;; 4th arg is a device object to apply the changes
(set-input-mode t nil t)

;; mode-line-inverse-video is obsolete; this is actually a better abstraction.
;; Supposedly it only works with tty domains.  What happens in X?
(set-face-reverse-p 'modeline nil)

;; this is called rsz-minibuf.el in xemacs, rsz-mini.el in emacs.
(resize-minibuffer-mode 1)


;;;;;;
;;; External libraries to load
;;;;;;

(add-forms-to-after-load-alist
  '(("dbfrobs"
     (dbfrobs:cancel-debug-on-condition 'undefined-keystroke-sequence t))))

(add-after-load-libraries
  "dbfrobs"
  "efs-auto"
  "vh-scroll")

(provide 'init-xemacs-19)

;;; init-xemacs-19.el ends here.
