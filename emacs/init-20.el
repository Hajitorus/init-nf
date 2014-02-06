;;; init-20.el --- startup for Emacs version 20

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1997-10-14
;; Public domain.

;; $Id: init-20.el,v 1.44 2011/12/05 21:52:27 friedman Exp $

;;; Commentary:
;;; Code:

(require 'init)
(require 'frame-fns)

;;;;;;
;;; Defuns
;;;;;;

;; ----------
(defalias-undefined 'characterp 'integerp)

;; ----------
(defun-undefined set-frame-parameter (frame param value)
  "Set frame parameter PARAMETER to VALUE on FRAME.
If FRAME is nil, it defaults to the selected frame.
See `modify-frame-parameters'."
  (modify-frame-parameters frame (list (cons param value))))


;;;;;;
;;; Mode hooks
;;;;;;


;;;;;;
;;; Variables
;;;;;;

(setq add-log-keep-changes-together t
      ;compilation-scroll-output    t
      initial-scratch-message       nil
      ring-bell-function            'ignore)

(setq backward-delete-char-untabify-method 'untabify)

;; In emacs 20.4, this seems to be the default when starting emacs with x
;; window system connectivity.  But starting on a tty, it's set to t.
;; Whenever you call set-buffer-multibyte to change the value of a given
;; buffer's encoding, the undo list is discarded.  Since VM calls this in
;; various circumstances to disable multibyte encoding temporarily, it's
;; less pain for me to have this off by default.
(when (= (emacs-version-major) 20) ; trying something different in Emacs 21.
  (setq-default enable-multibyte-characters nil))

;; Don't use the mule char encoding stuff for CRLF handling; use crypt++
;; instead which makes it far easier to convert buffers to linefeed-only.
;;
;; Another alternative is to leave this variable set to nil and to convert
;; buffers to unix linefeed endings, use `set-buffer-file-coding-system' to
;; set the current coding system to `ctext-unix'.  When you save, carriage
;; returns will not be added back to the lines.
(setq-default inhibit-eol-conversion t)

(setq scroll-preserve-screen-position t)


;;;;;;
;;; Key bindings
;;;;;;


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(unless (featurep 'init-19)
  (load-offer-compile "init-19"))

;; Do not corrupt binary data.
(mapc (lambda (pat)
        (when (consp pat)
          (setq pat (format "\\.\\(?:%s\\)\\'"
                            (mapconcat 'identity pat "\\|"))))
        (modify-coding-system-alist 'file pat 'no-conversion))
  '(;; audio formats
    ("aac" "aiff" "au" "flac" "m4a" "mp[0-9]" "r[am]" "wav")
    ;; image formats
    ("gif" "[jm]pe?g" "png" "psd" "tiff?" "xcf")
    ;; multimedia formats
    ("avi" "mov" "qt" "swf" "wmv")
    ;; document formats
    ("dvi" "pdf" "ps")
    ;; archive formats
    ("arc" "cb[rz]" "cpio" "deb" "lzh" "pa[kx]" "rpm" "[jrt]ar" "tgz" "xpi" "zip")
    "\\.tar\\."
    ;; openoffice formats
    ("s[tx][cdiw]" "vor")
    ;; msft formats
    ("doc" "ppt" "xls" "bat" "com" "exe")
    ;; palmos formats
    ("pdb" "prc")
    ;; encrypted files
    ("gpg" "pgp")
    ;; filesystem images
    ("v?fat" "msdos" "ntfs" "[feux]fs" "ext[23]\\(?:fs\\)?"
     "reiser\\(?:fs\\)?" "dvd" "img" "iso" "udf")
    ;; misc binary data formats
    ("a" "bci" "bin" "s?o" "so\\.[0-9.]+" "x?fasl")))

;; Emacs 19 did not have cperl-mode as a standard package, but emacs 20 does.
;; Make it the default rather than perl-mode.
(defalias 'perl-mode 'cperl-mode)

(for-frame-type (x) use-terminus-font
  (when (x-list-fonts "-*-terminus-medium-r-normal-*-12-*-*-*-c-60-*-1")
    (make-variable-frame-local 'make-large-simple-frame-fontset)
    (set-frame-parameter nil 'make-large-simple-frame-fontset 'terminus-noah)))

(when (= (emacs-version-major) 20)
  (require 'latin-1)
  (standard-display-european 1)
  (disptable-insert-w32/palmos-8bit-glyphs standard-display-table))

;; fixup resource name when invoking emacs as "-emacs".
(when (and (eq window-system 'x)
           (equal ?- (aref x-resource-name 0)))
  (setq x-resource-name (substring x-resource-name 1 )))

(temp-buffer-resize-mode 1)


;;;;;;
;;; External libraries to load
;;;;;;

(add-forms-to-after-load-alist
  '(("battery"
     (load-offer-compile "init-battery" t))

    ("file-fns"
     (add-hook 'before-save-hook 'nf-backup-buffer-if-mtime-elapsed))

    (frame-fns
     (set-alist-slot 'make-large-simple-frame-fontset-alist
      'terminus-noah
      '((cond
         ((>= display-height 1200) "-*-terminus-medium-r-normal--20-*-*-*-c-100-*-1")     ;; 10x20
         ((>= display-height 1024) "-*-terminus-medium-r-normal--14-*-*-*-c-80-*-1")      ;;  8x14
         (t                        "-*-terminus-medium-r-normal--12-*-*-*-c-60-*-1")))))  ;;  6x12

    ("win-disp-util"
     (add-hook 'temp-buffer-setup-hook 'wdu-temp-buffer-completion-save-point))
    ))

;;(add-after-load-libraries nil)

(remove-after-load-libraries
  "show-temp-buffer"
  "scroll-in-place")

(provide 'init-20)

;;; init-20.el ends here.
