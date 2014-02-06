;;; init-19-only.el --- startup for emacs version 19.x only

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1997
;; Public domain.

;; $Id: init-19-only.el,v 1.2 2011/10/10 08:16:43 friedman Exp $

;;; Commentary:
;;; Code:

(require 'init)
(require 'init-19)
(require 'frame-fns)

;;;;;;
;;; Variables
;;;;;;

;; Emacs 20 has this already, and various recent packages want it.
(defvar temporary-file-directory
  (file-name-as-directory
   (cond ((memq system-type '(ms-dos windows-nt))
	  (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
	 ((or (getenv "TMPDIR") "/tmp"))))
  "The directory for writing temporary files.")

;;;;;;
;;; Macros
;;;;;;

;; ----------
;; Needed for Emacs 19.34 and earlier, and XEmacs 19.14 and earlier.
;; This doesn't implement any customs support, but at least it makes modern
;; packages loadable in those older versions.
(init-eval-and-compile-unless (featurep 'custom)
  (load "custom" t)
  (load "cust-stub" t)
  (unless (fboundp 'defcustom)
    (defmacro defgroup (&rest args) nil)
    (defmacro defcustom (var value doc &rest args)
      (list 'defvar var value doc))))

;;;;;;
;;; Defuns
;;;;;;

;; ----------
;; Emacs 19 didn't have these; Emacs 20 and later do.
(mapc (lambda (fn)
        (unless (fboundp fn)
          (fset fn (make-general-car-cdr fn))))
  '(cadr cddr))

;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(when (require-soft 'iso-syntax)
  (standard-display-european 1)
  (disptable-insert-w32/palmos-8bit-glyphs standard-display-table))

(for-frame-type (x) use-terminus-font
  (when (x-list-fonts "-*-terminus-medium-r-normal-*-14-*-*-*-c-80-*-1")
    (setq make-large-simple-frame-fontset 'terminus)))

(provide 'init-19-only)

;;; init-19-only.el ends here.
