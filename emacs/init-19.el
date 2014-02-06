;;; init-19.el --- startup for emacs version 19

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1992
;; Public domain

;; $Id: init-19.el,v 1.113 2011/12/05 21:52:27 friedman Exp $

;;; Commentary:
;;; Code:

(require 'init)
(require 'list-fns)
(require 'face-fns)

;;;;;;
;;; Variables
;;;;;;

(defvar original-input-mode (current-input-mode))

(defvar default-window-system-frame-font-alist
  '((x . (or (car (x-list-fonts
                   (cond
                    ((>= (x-display-pixel-height) 1200)
                     "-*-terminus-medium-r-normal-*-14-*-*-*-c-80-*-1")
                    (t
                     "-*-terminus-medium-r-normal-*-12-*-*-*-c-60-*-1"))))
             "fixed"))
    (ns  . "-*-Terminus-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
    (w32 . "-*-courier new-normal-r-*-*-12-*-*-*-c-*-iso8859-1"))
  "Font defaults for frames created on specific kinds of window systems.
These are activated via the `insert-default-font' advice on `make-frame'
as returned by the function `default-window-system-frame-font'.

Values are of the form (WINSYS . FONT),
  where WINSYS is the symbol representing the type of window system as used
  in the `window-system' global value or frame parameter,
  and FONT is a string or sexp to be evaluated to return the font string.")

(defvar after-command-line-1-hook nil
  "Hook to run after all else in `command-line-1'.
That is the last function to run before normal user interaction begins at
startup and is a convenient place to put overrides to command line
processing and other initializations that occur after ordinary startup
hooks have run.")

;;;;;;
;;; Macros
;;;;;;

;; ----------
(init-eval-and-compile-unless (fboundp 'unless)
  (defmacro unless (pred &rest body)
    (cons 'if (cons pred (cons nil body)))))

;; ----------
(init-eval-and-compile-unless (fboundp 'when)
  (defmacro when (pred &rest body)
    (list 'if pred (cons 'progn body))))

;; ----------
;; Written by Roland McGrath <roland@frob.com>.
;; See init-18.el for an earlier version which works with emacs 18.
;; This version is just some syntactic sugar for defadvice.
(defmacro make-interactive (symbol &rest interactive-args)
  "Make the function definition of SYMBOL an interactive command.
Remaining arguments, if any, are passed to `interactive' in the function.

This is a macro and SYMBOL must be a literal atom but for hysterical
raisins it is usually called like: (make-interactive 'SYMBOL ...)
In version 19 it also works to omit the quote form, but the version 18
function cannot support that."
  (if (eq (car-safe symbol) 'quote)
      (setq symbol (car (cdr symbol))))
  `(defadvice ,symbol (before sinit:make-interactive activate compile)
     "You can run this function as a command or bind it to a key."
     (interactive ,@interactive-args)))

;; ----------
;; Clever way to swap variables without use of an explicit temporary variable.
;; Adapted from HAKMEM (MIT AI Memo #239) item 163 (Sussman)
;;
;; Note that if A and B are integers, it is possible to swap values without
;; using any temporary variables or stack space:
;;
;;  (setq a (logxor a b))
;;  (setq b (logxor a b))
;;  (setq a (logxor a b))
(defmacro swap (a b)
  "Swap the values of variables A and B."
  `(setq ,a (prog1 ,b (setq ,b ,a))))

;;;;;;
;;; Defuns
;;;;;;

;;----------
(defalias-undefined 'mapc 'for-each)
(put 'mapc 'lisp-indent-function 1)

;; ----------
;; In XEmacs, char-syntax has a table arg already.
(defun char-syntax-in-table (char &optional table)
  "Like char-syntax, but optionally specify syntax table."
  (let ((otbl (syntax-table)))
    (unwind-protect
        (progn
          (set-syntax-table (or table otbl))
          (char-syntax char))
      (set-syntax-table otbl))))

;; ----------
;; This is an XEmacs function; in Emacs, chars and ints are already eq.
(defalias-undefined 'char-to-int 'identity)

;; ----------
(defadvice command-line-1 (after sinit:after-command-line-1-hook activate)
  "Run 'after-command-line-1-hook' after all else."
  (run-hooks 'after-command-line-1-hook))

;; ----------
;; Disable forced annoying message in emacs 19.23 and later.
;;
;; Put this on the *end* of after-do-inits-hook.
;; This should not be called until everything else is done.  Otherwise, if
;; there is an error in these init files, and noninteractive has been set,
;; command-loop (see startup.el) will kill emacs because command-loop-1
;; never finishes processing the arg that would clear it.
;;
;; Safer for emacs 21 and later; the above caveat no longer applies.
(defun defeat-startup-echo-area-message ()
  (cond (noninteractive)
        ((string-lessp "21" emacs-version)
         ;; Less perilous version
         (put 'inhibit-startup-echo-area-message 'saved-value t)
         (setq inhibit-startup-echo-area-message
               (if (string= init-file-user "")
                   (user-login-name)
                 init-file-user)))
        (t
         (setq noninteractive 'rms-is-a-stinking-fascist)
         (defun init-clear-noninteractive ()
           (setq noninteractive nil)
           (fmakunbound 'init-clear-noninteractive))
         (setcdr command-line-args (nconc '("-f" "init-clear-noninteractive")
                                          (cdr command-line-args))))))

;; ----------
(defun default-window-system-frame-font (&optional winsys)
  "Return default font to use for frames on window system WINSYS.
WINSYS may be an existing frame, in which case that frame's window-system is examined.

Fonts are chosen via `default-window-system-frame-font-alist'.

This function is used by the `sinit:insert-default-font' defadvice to `make-frame'."
  (cond ((framep winsys)
         (setq winsys (cdr (assq 'window-system (frame-parameters winsys)))))
        ((null winsys)
         (setq winsys window-system)))

  (let ((font (cdr (assq winsys default-window-system-frame-font-alist))))
    (cond ((null font)
           nil)
          ((stringp font)
           font)
          (t
           (eval font)))))

;; ----------
;; This assumes that the grep command includes -H
(defadvice grep (around sinit:no-null-device activate)
  "Do not put /dev/null on end of command line since that breaks piplines."
  (interactive
   (let ((null-device nil))
     (eval (cadr (ad-interactive-form
                  (ad-get-orig-definition this-command))))))
  ad-do-it)

;; ----------
(defadvice make-frame (before sinit:insert-default-font activate)
  "When new frames are created with no font specified, use default font as
determined by the `default-window-system-frame-font' function.

This will cause an override of any font specified in `default-frame-alist'."
  (let* ((params (ad-get-arg 0))
         (winsys (cdr (assq 'window-system params)))
         (deffont nil))
    (when (not (assq 'font params))
      (setq deffont (default-window-system-frame-font winsys))
      (if deffont
          (ad-set-arg 0 (cons (cons 'font deffont) params))))))


;; ----------
(defadvice make-frame (before sinit:no-tty-multiple-frames activate)
  "Do not make new tty frames on the same tty as existing frame.
It's too confusing."
  (unless (or window-system
              (assq 'display (ad-get-arg 0))
              (assq 'tty     (ad-get-arg 0)))
    (error "Don't create multiple frames on the same tty")))

;; ----------
(defun make-tall-frame (&optional color display)
  (interactive)
  (let ((x-offset (if (= (x-display-pixel-width) 1024)
                      400
                    -101))) ; width of twm iconmgr + border
    (make-large-simple-frame 80 nil x-offset nil nil color display)))

;; ----------
(defun make-tall-frame-on-display (&optional color display)
  (interactive (list nil (x-display-completing-read
                          "Make tall frame on display: ")))
  (make-tall-frame color display))

;; ----------
(defun make-large-simple-frame-on-display (&optional color display)
  (interactive (list nil (x-display-completing-read
                          "Make full frame on display: ")))
  (make-large-simple-frame nil nil nil nil nil color display))

;; ----------
(defadvice other-window (around sinit:notify-one-window activate)
  (if (one-window-p)
      (message "No other window")
    ad-do-it))

;; ----------
;; This is an XEmacs function; keymap names are only there for printing and
;; debugging purposes so this is a reasonable definition.
(defalias-undefined 'set-keymap-name 'ignore)

;; ----------
(defadvice tar-clip-time-string (around sinit:iso-timestamp activate)
  "Show ISO-format timestamps when tar-mode-show-date is non-nil."
  (cond ((not (fboundp 'format-time-string))
         ad-do-it)
        ((and (eq (emacs-variant) 'emacs)
              (>= (emacs-version-major) 21))
         (setq ad-return-value
               (format-time-string " %Y-%m-%d %H:%M" (ad-get-arg 0))))
        (t
         (setq ad-return-value
               (format-time-string "%Y-%m-%d %H:%M" (ad-get-arg 0))))))

;; ----------
;; XEmacs has a function of this name.  It strips the trailing slash.
(defun temp-directory ()
  "Return the pathname to the directory to use for temporary files."
  (directory-file-name temporary-file-directory))


(load-offer-compile "init-common")
(load-offer-compile "init-vc" t)


;;;;;;
;;; Hooks
;;;;;;


;;;;;;
;;; Variables
;;;;;;

(setq-default buffer-percentage-mode  t
              line-number-mode        nil
              column-number-mode      nil)

;; For now, use the default values, which should be t.
;;
;; (and (featurep 'hack-locals)
;;      (setq enable-local-variables 'query
;;            enable-local-eval      'query))

(setq auto-save-timeout               15
      find-file-existing-other-name   t
      ;; Created in 19.31 to control whether messages about GC are printed.
      garbage-collection-messages     t
      ;; Introduced in Emacs 19.29
      message-log-max                 (expt 2 15)
      next-line-add-newlines          nil
      ;; Implemented in emacs 19.30.
      ;; This feature displays keybindings for extended commands before
      ;; they are executed; I prefer using a function on
      ;; after-execute-extended-command-hook to print the keybinding after
      ;; the fact.
      suggest-key-bindings            nil
      system-uses-terminfo            nil  ; prevent comint.el from losing
      tar-mode-show-date              t
      x-select-enable-clipboard       t    ; introduced in 19.23
      )

(setq undo-limit                      (* 10 megabyte)
      undo-strong-limit               (* 20 megabyte))

(setq frame-title-format (list "" (format "%s%c%s"
                                          (symbol-name (emacs-variant))
                                          (if (zerop (user-uid)) ?# ?@)
                                          (abbreviate-hostnick system-name)))
      icon-title-format frame-title-format)

;;; frame-alist munging

;; Emacs 24 leaves this variable completely nil.
(unless default-frame-alist (setq default-frame-alist '(nil)))

(let ((alist default-frame-alist))
  ;; font not specified here because that creates problems with
  ;; multi-tty emacs implementations.  See the `insert-default-font'
  ;; advice on `make-frame' for alternate mechanism.

  ;; title and icon-name are new as of 19.31.
  (set-alist-slot alist 'name                   nil)
  (set-alist-slot alist 'title                  nil)
  (set-alist-slot alist 'icon-name              nil)

  (set-alist-slot alist 'background-mode        'dark)   ; new in 21.1
  (set-alist-slot alist 'background-color       "black")
  (set-alist-slot alist 'foreground-color       "white")
  (set-alist-slot alist 'border-color           "black")
  (set-alist-slot alist 'cursor-color           "white")
  (set-alist-slot alist 'mouse-color            "white")

  (set-alist-slot alist 'height                 24)
  (set-alist-slot alist 'width                  80)
 ;(set-alist-slot alist 'left                   0)
 ;(set-alist-slot alist 'top                    0)

  ;; you can't set the border-width while emacs is starting up
  ;; because both the initial-frame-alist and default-frame-alist
  ;; are used to reset the frame parameters before the normal repl
  ;; begins.  You can't modify the border width of an existing
  ;; frame.  (Is this a window manager specific problem?)
  ;;
  ;; In Emacs 19, the default border width is 2.  In Emacs 20, the
  ;; default border width is 1.  Setting the Emacs.borderWidth X
  ;; resource does not seem to help.
 ;(set-alist-slot alist 'border-width           1)
 ;(set-alist-slot alist 'internal-border-width  2)

  (set-alist-slot alist 'auto-lower             nil)
  (set-alist-slot alist 'auto-raise             nil)
  (set-alist-slot alist 'menu-bar-lines         0)
  (set-alist-slot alist 'tool-bar-lines         0) ; for emacs 21.x
  (set-alist-slot alist 'minibuffer             t)
  (set-alist-slot alist 'unsplittable           nil)
  (set-alist-slot alist 'visibility             t) ; nil or 'icon

  (set-alist-slot alist 'cursor-type            'box) ; 'bar
  (set-alist-slot alist 'icon-type              nil)

  (set-alist-slot alist 'vertical-scroll-bars   nil) ; 'left or 'right
 ;(set-alist-slot alist 'horizontal-scroll-bars nil)  ; not implemented
  (set-alist-slot alist 'scroll-bar-width       12)
  )

;; Delete null cons added for emacs 24 workaround above.
(delq nil default-frame-alist)

;; initial-frame-alist gets all the values of default-frame-alist
;; plus we specify an initial font.
(let ((alist (copy-alist default-frame-alist))
      (font (default-window-system-frame-font)))
  (when font
    (set-alist-slot alist 'font font))
  (setq initial-frame-alist alist))

;; 1st arg eq t => use raw mode (nil means cbreak)
;; 2nd arg eq t => use C-s/C-q flow control (only matters in cbreak mode)
;; 3rd arg eq t => use meta bit; other non-nil => input is 8-bit chars
;; 4th arg is quit character.
(set-input-mode t nil t ?\C-g)


;;;;;;
;;; Key bindings
;;;;;;

;; Starting with Emacs 19.29, ESC-ESC is no longer bound to eval-expression.
;; Instead, ESC-ESC-ESC is bound to keyboard-escape-quit, which does
;; incredibly annoying things like delete all your windows but one,
;; undefine the region, and other things that throw away useful state.
;; I don't particularly need that function and although M-: has actually
;; turned out to be easier to type, I still revert to old habits sometimes
;; (especially on keyboards with no meta key).  So put back eval-expression.
;;
;; 1998-03-03: I never use ESC-ESC anymore, and leaving that sequence free
;; supposedly makes some function keys work better.
;(keymap-undefine-keys nil "\e\e\e")
;(keymap-define-keys nil '(("\e\e" . eval-expression)))

(keymap-define-keys nil
  '(("\M-\C-i"  .   lisp-complete-symbol)
    ("\C-c\C-e" .   eval-pretty-print-last-sexp)
    ("\C-x5o"   .   other-frame)
    ("\C-x5O"   .   other-frame-absolute)
    ;; This is for symmetry with the old ESC-ESC and C-x-ESC-ESC bindings.
    ("\C-x\M-:" .   repeat-complex-command)))


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (and window-system
           (string-lessp emacs-version "21"))
  (global-font-lock-mode 1)
  (setq font-lock-support-mode 'lazy-lock-mode)
  ;; This must be done after enabling global-font-lock-mode for the
  ;; first time.
  (disable-mode-font-lock 'shell-mode))

(when (boundp 'auto-save-list-file-prefix)
  (setq auto-save-list-file-prefix
        (if (file-directory-p "~/.emacs.d")
            "~/.emacs.d/auto-save-list/.saves-"
          "/tmp/.saves-")))

;; important caveat about this; see function defn
(add-hook 'after-do-inits-hook 'defeat-startup-echo-area-message 'append)

(put 'modify-frame-parameters 'lisp-indent-function 1)


;;;;;;
;;; External libraries to load
;;;;;;

(add-forms-to-after-load-alist
  '(("font-lock"
     (mapc 'disable-mode-font-lock
       '(comint-mode shell-mode ssh-mode rlogin-mode ftelnet-mode)))
    ))

(add-after-load-libraries
  '(load-offer-compile "dbfrobs")
  '(load-offer-compile "eldoc")
  '(load-offer-compile "flash-paren")
  '(load-offer-compile "frame-fns")
  "rsz-mini"
  "vh-scroll")

(cond ((or (memq system-type '(linux lignux gnu/linux))
           (system-configuration-matches-p "linux" "redhat" "debian"))
       (add-after-load-libraries "linuxproc")))

(provide 'init-19)

;;; init-19.el ends here.
