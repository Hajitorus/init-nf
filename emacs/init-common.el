;;; init-common.el --- common startup for all versions of Emacs and XEmacs

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1991
;; Public domain

;; $Id: init-common.el,v 1.230 2011/12/05 21:52:27 friedman Exp $

;;; Commentary:

;; This file doesn't actually work with Emacs 18 anymore.
;; Probably versions prior to 19.29 won't work either.

;;; Code:

(require 'buffer-fns)
(require 'host-fns)
(require 'kbd-fns)
(require 'list-fns)
(require 'motion-fns)

(provide 'init-common)


;;;;;;
;;; Personal variable declarations
;;; These are not variables defined in other packages, but rather they are
;;; declarations for variables I created and use in my initialization files.
;;;;;;

;; ----------
(defconst emacs-launch-time (current-time)
  "Time at which this emacs instance was launched.
Technically, it is the time the file was loaded which contains this
variable declaration, but that file is usually loaded as part of my emacs
initialization.")

;; ----------
;; I got so damned tired of completion in the shell buffer offering to
;; complete on useless things like `.' and `..' when there's only one other
;; file in the directory.  Like I'd actually want to complete on those.
;; I'm also not interested in completing on scm metadata dirs unless I
;; specifically want to edit something in them.
(defvar file-name-all-completions-ignore
  '("./" "../" ".svn/" "CVS/" "RCS/" "{arch}/")
  "*Entries which are removed from the list returned by \
`file-name-all-completions' when completing all directory entries.")

;; ----------
(defvar global-mode-string-trailer nil
  "*Text to add to the rightmost end of mode-line-format.
This variable is used by set-default-mode-line-format.")

;; v22 has `most-positive-fixnum'; use that if defined since emacs might
;; (hopefully!) someday have bignums, in which case the lsh hack won't
;; work.
(defconst moby-bignum
  (if (boundp 'most-positive-fixnum)
      most-positive-fixnum
    (lsh (lsh -1 1) -1))
  "Poor man's infinity.")

(defconst int32-max (if (zerop (lsh -1 31)) ; VALBITS < 32
                        moby-bignum         ; biggest lisp integer < INT_MAX
                      (1- (lsh 1 31)))      ; cap it at 2^31-1
  "Maximum 32-bit-limited value.
On machines with VALBITS > 32, variables that are defined in C with
DEFVAR_INT are still of type `int', which is usually (e.g. on the 64-bit
Alpha) still only 32 bits.  Calculate the maximum value for such
variables.")

(defvar megabyte (* 1024 1024))

(defvar nf-init-shell-prompt-patterns
  '("[^\n#$%<>();?]*[#$%>;?]+"                   ; bourne/csh
    "([a-z]+)"                                   ; gdb
    "[ \t]*DB<[0-9]+>"                           ; perl debugger
    "[^]\n*:]*[]*:]"                             ; sbcl debugger
    ))


;;;;;;
;;; Macros
;;;;;;


;;;;;;
;;; Defuns (some are modified or extended versions of standard functions)
;;;;;;

;; ----------
(defun common-process-mode-inits ()
  (make-local-variable 'undo-limit)
  (make-local-variable 'undo-strong-limit)
  (setq undo-limit        (* 1 megabyte))
  (setq undo-strong-limit (* 2 megabyte))

  (and (fboundp 'buffer-percentage-mode)
       (buffer-percentage-mode -1))
  (and (fboundp 'protect-process-buffer-from-kill-mode)
       (protect-process-buffer-from-kill-mode 1)))

;; ----------
(defun enable-and-run-command (&rest args)
  (put this-command 'disabled nil)
  (call-interactively this-command))

;; ----------
(defadvice file-name-all-completions (after sinit:strip-ignored-dirs activate)
  "Strip names from returned list per `file-name-all-completions-ignore'."
  (let ((fn (lambda (elt)
              (setq ad-return-value (delete elt ad-return-value)))))
    (cond ((and (consp ad-return-value)
                (null (cdr ad-return-value))))
          (t
           (mapc fn '("./" "../")) ; always remove these
           (when (string= (ad-get-arg 0) "")
             (mapc fn file-name-all-completions-ignore))))))

;; ----------
(defadvice file-name-completion (after sinit:strip-ignored-dirs activate)
  "Ignore names per `file-name-all-completions-ignore' unless they are the only match."
  (let ((result ad-return-value))
    (and (stringp result)
         (string= result "")
         (setq result (file-name-all-completions
                       (ad-get-arg 0) (ad-get-arg 1))))
    (when (and (consp result)
               (null (cdr result)))
      (if (string= (car result) (ad-get-arg 0))
          (setq ad-return-value t)
        (setq ad-return-value (car result))))))

;; ----------
(defun gc-print-statistics ()
  "Do a garbage collection and print statistics in the minibuffer."
  (interactive)
  (message "%s" (garbage-collect)))

;; ----------
(defadvice kill-emacs (around sinit:interactive-save-buffers-query activate)
  "If called interactively, behave like `save-buffers-kill-emacs'."
  (if (interactive-p)
      (call-interactively 'save-buffers-kill-emacs)
    ad-do-it))

;; ----------
(defun nf-init-make-shell-prompt-pattern (&rest patterns)
  (format "^\\(%s\\)[ \t]*"
          (mapconcat 'identity
                     (or patterns nf-init-shell-prompt-patterns)
                     "\\|")))

;; ----------
(defun my-matching-paren (c)
  "Return the matching parenthesis of CHAR, or nil if none."
      (and (memq (char-syntax c) '(?\( ?\)))
           (lsh (aref (syntax-table) c) -8)))

;; matching-paren is a subr in emacs 19.26 and later.
(or (fboundp 'matching-paren)
    (defalias 'matching-paren 'my-matching-paren))

;; ----------
(defun override-default-variable-settings ()
  "User defined function.  Intended to be called within various hooks to
override the value of buffer-local variables whose default values
might have been overridden by the major mode."
  (set-tab-stop-width 2))

;; ----------
(defun rename-rhost-buffer ()
  (let ((newname (re-substring "^\\*[^-]+-\\(.*\\)\\*$" (buffer-name) 1)))
    (cond (newname
           (setq newname (concat "*" newname "*"))
           (cond ((>= (emacs-version-major) 19)
                  (rename-buffer newname t))
                 (t
                  (or (get-buffer newname)
                      (rename-buffer newname))))))))

;; ----------
(defadvice revert-buffer (before sinit:interactive-noconfirm activate)
  "If called interactively on an unmodified buffer, do not query before reverting."
  (and (interactive-p)
       (not (buffer-modified-p))
       (ad-set-arg 1 t)))

;; ----------
(defun scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; ----------
(defun set-default-mode-line-format ()
  (interactive)
  (setq-default mode-line-hostname (abbreviate-hostnick (system-name)))
  (setq-default mode-line-modified
                (cond ((string-lessp emacs-version "20") '("%1+%1*"))
                      ((string-lessp emacs-version "21")
                       '("%1&" (buffer-read-only "%%")))
                      (t '(:eval (if (and buffer-file-name (buffer-modified-p))
                                     '("*" (buffer-read-only "%%"))
                                   '("%1*"))))))
  (setq-default mode-line-buffer-ident-prefix nil)
  (setq-default mode-line-buffer-identification
                (list ""
                      'mode-line-hostname
                      (format "%c%%z" (if (= (user-uid) 0) ?# ?:))
                      '(current-input-method-title
                        ("X" "[" current-input-method-title "]") "")
                      " %b"))
  (setq-default mode-line-position
                '((line-number-mode
                   (column-number-mode "[%l,%c] " "L%l ")
                   (column-number-mode "C%c "))
                  (buffer-percentage-mode ("" (-3 . "%p") " "))
                  (size-indication-mode "%I ")))
  (setq-default mode-line-modes
                '("%[("
                  mode-name
                  mode-line-process
                  "%n"
                  minor-mode-alist
                  ")%]"))
  (setq-default mode-line-format
                '(""
                  ;mode-line-mule-info ; see mode-line-buffer-identification
                  mode-line-modified
                  ;mode-line-frame-identification
                  mode-line-buffer-ident-prefix
                  mode-line-buffer-identification
                  " " global-mode-string " "
                  mode-line-position
                  (escreen-number-mode escreen-mode-line-format)
                  mode-line-modes
                  (which-func-mode (" " which-func-format ""))
                  (buffer-directory-file-name
                   (" " buffer-directory-file-name
                        (global-mode-string-trailer "" " ")))
                  global-mode-string-trailer
                  "%-")))

;; ----------
(defun shell-mode-inits ()
  ;; shell-mode-hook is called after everything in shell-mode
  ;; except for the call to comint-read-input-ring.
  ;; As of emacs 19.26, insert-file-contents won't handle
  ;; anything but ordinary files, so setting HISTFILE to
  ;; /dev/null creates gratuitous errors.
  (cond ((not (boundp 'comint-input-ring-file-name)))
        ((file-plain-p comint-input-ring-file-name))
        (t
         (setq comint-input-ring-file-name nil)))
  (require-soft 'cdpath)
  (setq mode-name "shell")
  ;; This interferes with my fff.el, and I'd rather be able to
  ;; use that.
  (keymap-undefine-keys shell-mode-map "\C-c\C-f")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (fboundp 'set-process-query-on-exit-flag)
        ;; New as of emacs 22
        (set-process-query-on-exit-flag proc nil)
      (process-kill-without-query proc))))

;; ----------
(defun shell-process-mode-inits ()
  (require-soft 'proc-filters)
  (require-soft 'protbuf)

  (override-default-variable-settings)
  (make-local-variable 'scroll-step)
  (setq scroll-step 1)

  (common-process-mode-inits)

  ;; New in v22
  (remove-hook 'comint-output-filter-functions 'shell-filter-ctrl-a-ctrl-b t)

  (setq comint-prompt-regexp (nf-init-make-shell-prompt-pattern))

  (cond
   ((featurep 'proc-filters)
    (make-local-hook 'comint-output-filter-functions)
    (add-hook 'comint-output-filter-functions
              'proc-filter-shell-output-filter nil t)

    ;; Remove global hooks
    (remove-hook 'comint-output-filter-functions 'comint-carriage-motion)

    ;; Remove local hooks
    (mapc (lambda (sym)
            (remove-hook 'comint-output-filter-functions sym t))
      '(ftelnet-carriage-filter
        rlogin-carriage-filter
        ssh-carriage-filter))

    ;; New in 21.1; don't use this since we have our own filter.
    (setq comint-inhibit-carriage-motion t))))

;; ----------
(defadvice substitute-in-file-name (before init:subst-makefile-vars activate)
  "Handle $(FOO) like ${FOO}"
  (save-match-data
    (while (string-match "\\$(\\([^\)]+\\))" (ad-get-arg 0))
      (ad-set-arg 0 (replace-match "${\\1}" t nil (ad-get-arg 0))))))

;; ----------
(defun unset-display ()
  "Unset DISPLAY environment variable in process-evironment."
  (interactive)
  (setenv "DISPLAY" nil))


;;;;;;
;;; Hook initialization
;;;;;;

;; ----------
;; Implemented by dbfrobs.el
(add-hook 'after-debugger-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

;; ----------
(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

;; ----------
(add-hook 'c-mode-hook 'override-default-variable-settings)

;; As of 22.0 this is the new name for the variable;
;; disabled-command-hook becomes a defvaralias.
(if (boundp 'disabled-command-function)
    (setq disabled-command-function 'enable-and-run-command)
  (setq disabled-command-hook 'enable-and-run-command))

;; ----------
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "Elisp")
            (override-default-variable-settings)
            (define-key emacs-lisp-mode-map "\C-j" 'eval-print-last-sexp)))

;; ----------
(add-hook 'find-file-hooks 'set-buffer-directory-file-name)

;; ----------
(add-hook 'ftelnet-mode-hook 'shell-process-mode-inits)

;; ----------
(add-hook 'gdb-mode-hook 'protect-process-buffer-from-kill-mode)

;; ----------
(add-hook 'imenu-after-jump-hook 'beginning-of-line)

;; ----------
;; Note: Emacs 21.0 has the variable `confirm-kill-emacs' which can be
;; bound to `yes-or-no-p' or similar function to accomplish the same thing
;; as this hook.  So if that variable is non-nil, this function returns t
;; without making any query, so that the user is not queried twice.
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (cond ((and (boundp 'confirm-kill-emacs)
                        confirm-kill-emacs)
                   t)
                  (t (yes-or-no-p "Confirm kill-emacs ")))))

;; ----------
(add-hook 'lisp-interaction-mode-hook 'override-default-variable-settings)

;; ----------
(add-hook 'rlogin-mode-hook 'shell-process-mode-inits)

;; ----------
(add-hook 'sh-mode-hook 'abbreviate-sh-mode-name)

;; ----------
(add-hook 'shell-mode-hook 'shell-mode-inits)
(add-hook 'shell-mode-hook 'shell-process-mode-inits)

;; ----------
(add-hook 'ssh-mode-hook 'shell-process-mode-inits)

;; ----------
(add-hook 'telnet-mode-hook 'shell-process-mode-inits)

;; ----------
(add-hook 'text-mode-hook
          (lambda ()
            ;; Emacs 21.3 sets this buffer-local to t in text-mode.
            (setq require-final-newline
                  (default-value 'require-final-newline))

            ;; If for some reason the *scratch* buffer was killed earlier
            ;; and is recreated here because all other buffers have been
            ;; killed, then reset the major mode to emacs-lisp-mode.
            ;; But don't do so if buffer is already modified; that implies
            ;; that a manual change was requested.
            (if (and (equal (buffer-name) "*scratch*")
                     (not (buffer-modified-p)))
                (emacs-lisp-mode)
              (override-default-variable-settings)
              (turn-on-auto-fill))))

;; ----------
(add-hook 'write-file-hooks 'set-buffer-directory-file-name)

;; ----------
(mapc (lambda (hook)
        (add-hook hook 'rename-rhost-buffer))
  '(ftelnet-mode-hook
    rlogin-mode-hook
    ssh-mode-hook
    telnet-mode-hook))

;; ----------
(mapc (lambda (hook)
        (add-hook hook 'common-process-mode-inits))
  '(ftelnet-mode-hook
    shell-mode-hook
    rlogin-mode-hook
    ssh-mode-hook
    telnet-mode-hook
    zenirc-mode-hook
    zenirc-dcc-chat-mode-hook
    zenicb-mode-hook))


;;;;;;
;;; Variables
;;;;;;

(load-offer-compile "init-path")
(load-offer-compile "init-mail" t)
(load-offer-compile "init-autoloads" t)

;; Set defaults of various buffer-local variables
(setq-default case-fold-search  t)
(setq-default fill-column       75)
(setq-default indent-tabs-mode  nil)
(setq-default selective-display nil)

;; Global variables
(setq blink-matching-paren-distance  (/ moby-bignum 2)
      default-ctl-arrow              t
      default-major-mode             'text-mode
      diff-switches                  "-bu"
      enable-recursive-minibuffers   t
      fill-column                    75
      gc-cons-threshold              (/ megabyte 2)
      goal-column                    nil
      inhibit-startup-message        t
      initial-major-mode             'emacs-lisp-mode
      line-number-display-limit      int32-max
      manual-program                 "man"
      max-lisp-eval-depth            2048
      max-specpdl-size               2048
      mode-line-inverse-video        nil
      require-final-newline          'ask
      search-slow-speed              4800
      search-slow-window-lines       4
      split-window-keep-point        nil  ; minimize redisplay
      truncate-partial-width-windows nil
      window-min-height              2
      window-min-width               2)

(setq enable-local-variables         t
      enable-local-eval              t)

;; File backup and version control stuff. Middle backups are deleted
;; without asking me because if I have that many backups I won't care
;; or indeed even notice if I lose a few.
(setq version-control                t
      kept-old-versions              moby-bignum
      kept-new-versions              moby-bignum
      ;; This is the new name for trim-versions-without-asking,
      ;; renamed in emacs 19.26.
      delete-old-versions            t
      backup-by-copying              nil
      backup-by-copying-when-linked  t)

(setq byte-compile-verbose            t
      byte-optimize                   t
      byte-optimize-log               nil
      byte-compile-error-on-warn      nil
      byte-compile-delete-errors      t
      byte-compile-dynamic            nil
      byte-compile-dynamic-docstrings nil
      byte-compile-generate-call-tree nil
      byte-compile-warnings           nil)

;; ange-ftp.el
(setq ange-ftp-generate-anonymous-password  t
      ange-ftp-make-backup-files            t
      ange-ftp-disable-netrc-security-check t
      ange-ftp-binary-file-name-regexp      ".*")

;; autorevert.el
(setq auto-revert-verbose             nil
      auto-revert-stop-on-user-input  t
      auto-revert-check-vc-info       nil)

;; calendar.el
(setq all-hebrew-calendar-holidays    t
      all-christian-calendar-holidays t
      all-islamic-calendar-holidays   t)

;; comint.el
(setq-default
      comint-completion-autolist      nil
      comint-input-autoexpand         nil
      comint-input-ignoredups         t
      comint-input-ring-size          4096
      comint-prompt-regexp            "^[^#$%>;]*[#$%>;] ?"
      comint-scroll-show-maximum-output    nil
      comint-scroll-to-bottom-on-input     nil
      comint-scroll-to-bottom-on-output    nil

      ;; Emacs 21+ comint variables
      comint-use-prompt-regexp-instead-of-fields t
      comint-highlight-input          nil
      comint-highlight-prompt         nil)

;; grep.el
(setq grep-command                    "egrep -Hine "
      grep-use-null-device            nil)


;;;;;;
;;; Key bindings
;;;;;;

(keymap-define-keys ctl-x-map
  '(("nd"        .   narrow-to-defun)
    ("nn"        .   narrow-to-region)
    ("np"        .   narrow-to-page)
    ("nr"        .   narrow-to-regexp)
    ("ns"        .   narrow-to-sexp)
    ("nw"        .   widen)))

(keymap-define-keys nil
  '(("\C-?"          .   backward-delete-char-untabify)
    ("\C-c)"         .   blink-matching-open)
    ("\C-c["         .   backward-paragraph)
    ("\C-c]"         .   forward-paragraph)
    ("\C-c\C-i"      .   overwrite-mode)
    ("\C-c\C-l"      .   center-line)
    ("\C-c\C-r"      .   replace-regexp)
    ("\C-c\C-v\C-f"  .   vm-visit-folder)
    ("\C-c\C-v\C-m"  .   vm)
    ("\C-cA"         .   add-change-log-entry)
    ("\C-ca"         .   add-change-log-entry-other-window)
    ("\C-cb"         .   bury-buffer)
    ("\C-cg"         .   goto-line)
    ("\C-cj"         .   imenu)
    ("\C-cl"         .   count-lines-page)
    ("\C-cq"         .   comment-out-region)
    ("\C-cr"         .   replace-string)
    ("\C-cs"         .   send-invisible)
    ("\C-cv"         .   shrink-window)
    ("\C-h"          .   help-command)
    ("\C-r"          .   isearch-backward-regexp)
    ("\C-s"          .   isearch-forward-regexp)
    ("\C-w"          .   backward-kill-word)))

(keymap-define-keys nil
  '(("\M-$"          .   ispell-word)
    ("\M-%"          .   query-replace-regexp)
    ("\M-:"          .   eval-expression) ; standard in 19.29 or later
    ;; Defining these prevents function keys from working in terminals.
    ;; Put them on C-c]/C-[ instead instead, above.
    ;("\M-["         .   backward-paragraph)
    ;("\M-]"         .   forward-paragraph)
    ("\M-\C-m"       .   other-window-directionally)
    ("\M-\C-w"       .   kill-region)))

;; See comments in after-load-alist entry for an explanation of why I do this.
(and (boundp 'text-mode-map)
     (define-key text-mode-map "\M-\C-i" 'lisp-complete-symbol))

;; Mercifully unbind these.
(keymap-unbind-commands 'set-goal-column 'set-fill-column)

;; Make sure ^Z and ^X^Z do nothing on window-system frames.
;; The command bound to these keys has changed over time and between
;; variants, so find whatever is bound currently and advise it.
(when (> (emacs-version-major) 18)
  (let ((map (current-global-map))
        (command nil))
    (mapc (lambda (key)
            (when (setq command (lookup-key map key))
              (eval
               `(defadvice ,command
                  (around sinit:no-winsys-suspend compile activate)
                  "Do not iconify window-system frames."
                  (if window-system
                      (message "Not suspending or iconifying window frame")
                    ad-do-it)))))
      '("\C-z" "\C-x\C-z"))))


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

;; Set the PATH environment variable from the exec-path so that child
;; processes will inherit anything emacs uses.
(setenv "PATH" (mapconcat 'identity exec-path
                          (if (eq system-type 'windows-nt) ";" ":")))

(setenv "EDITOR" "ed")
(setenv "VISUAL" "ed")

;; Subprocesses, especially xterms, should not inherit screen session
;; related environment variables.
(setenv "STY" nil)

;; bash subshells should start with level 1.
(and (getenv "SHLVL")
     (setenv "SHLVL" "0"))

(replace-auto-mode "\\.\\(bat\\|cmd\\)\\'"              'dos-mode)
(replace-auto-mode "\\.p[lm]\\'"                        'perl-mode)
(replace-auto-mode "\\.py\\'"                           'python-mode)
(replace-auto-mode "\\.tar\\.\\([zZ]\\|gz\\|bz2\\)\\'"  'tar-mode)
(replace-auto-mode "\\.tar\\'"                          'tar-mode)
(replace-auto-mode "\\.tgz\\'"                          'tar-mode)
(replace-auto-mode "\\.bash\\'"                         'shell-script-mode)
(replace-auto-mode "\\.css\\'"                          'css-mode)
;; comic book archive formats; .cbz is zip, .cbr is rar.
(replace-auto-mode "\\.cb[rz]\\'"                       'archive-mode)
;; jar files are java archives, which are in zip format.
;; fbu files are Firefox Extension Backup Extensions (FEBE) archives,
;; which are also zip format.
;; apk files are android (google phone) application packages
(replace-auto-mode "\\.\\(jar\\|xpi\\|fbu\\|apk\\)\\'"  'archive-mode)
;; openoffice template and work files; also in zip format
(replace-auto-mode "\\.\\(s[tx][cdiw]\\)\\'"            'archive-mode)
(replace-auto-mode "\\<Jam\\(base\\|file\\|rules\\)\\'" 'jam-mode)
;; yum.conf and repo config files format
(replace-auto-mode "\\.repo\\'"                         'conf-mode)
;; doc-view mode annoys the fuck out of me when I visit these files.
(replace-auto-mode "\\.\\(?:PDF\\|DVI\\|pdf\\|dvi\\)\\'" 'fundamental-mode)

(buffer-enable-undo (window-buffer (minibuffer-window)))
(set-default-mode-line-format)
(set-tab-stop-width 2)

;; Fix the cwd if we're in an automounted directory.
(let ((cwd (expand-file-name ".")))
  (save-match-data
    (and (string-match "^/tmp_mnt/" cwd)
         (setq cwd (substring cwd (1- (match-end 0))))
         (file-directory-p cwd)
         (setq default-directory (file-name-as-directory cwd)))))

;; Fix home if symlink
(and (fboundp 'file-truename)
     (let ((home (file-truename (expand-file-name "~")))
           (cwd (file-truename (expand-file-name "."))))
       (and (string= home cwd)
            (setq default-directory "~/"))))


;;;;;;
;;; External libraries to load
;;;;;;

;; Override any previous autoloads and re-autoload these.
(override-autoload 'tar-mode "tar-mode")

;; Add forms to after-load-alist for various libraries.
;; Doing this instead of just loading once and modifying means that these
;; modifications will happen every time the library is loaded.
(add-forms-to-after-load-alist
  '(("agchat"
     (load-offer-compile "init-agchat" t))

    ("bbdb"
     (load-offer-compile "init-bbdb" t))

    ("bibl-mode"
     (load-offer-compile "init-bibl" t))

    ("cc-mode"
     (setq-default c-electric-flag nil)

     (if cc-imenu-c-generic-expression
         (nconc cc-imenu-c-generic-expression
           '((nil "\\<DEFUN\\s-*(\\s-*\"[^\n\"]*\"\\s-*,\\s-*\\([^\n,]+\\)," 1)
             ;; Put these in a submenu since they can sometimes
             ;; conflict with regular C functions, e.g. "concat",
             ;; which is really Fconcat, vs. regular concat C
             ;; function in emacs/src/fns.c
             ("DEFUN" "\\<DEFUN\\s-*(\\s-*\"\\([^\n\"]*\\)\"" 1)))))

    ("comint"
     ;; make sure my preferred binding for M-C-l is already there.
     (require-soft 'win-disp-util)
     (define-key comint-mode-map "\C-a" 'comint-bol)
     (define-key comint-mode-map "\M-\C-l" (global-key-binding "\M-\C-l"))

     (cond
      ((fboundp 'comint-previous-matching-input-from-input)
       (keymap-define-keys comint-mode-map
         '(("\M-p" . comint-previous-matching-input-from-input)
           ("\M-n" . comint-next-matching-input-from-input)
           ("\M-P" . comint-previous-input)
           ("\M-N" . comint-next-input)))))

     ;; 2004-11-19 Emacs 22 cvs sources have started enabling this hook
     ;; by default.  Very obnoxious.
     (remove-hook 'comint-output-filter-functions
                  'comint-watch-for-password-prompt)

     (or (require-soft 'comint-popup)
         (require-soft 'pb-popup)))

    ("comint-popup"
     (setq-default comint-popup-idle-threshold 30)
     (setq-default comint-popup-at-prompt-p    t)

     (defun comint-popup-add-popup-hook ()
       (make-local-hook 'comint-output-filter-functions)
       (add-hook 'comint-output-filter-functions
                 'comint-popup-buffer t t))

     (mapc (lambda (h)
             (add-hook h 'comint-popup-add-popup-hook t))
       '(gdb-mode-hook
         ftelnet-mode-hook
         rlogin-mode-hook
         shell-mode-hook
         ssh-mode-hook
         telnet-mode-hook)))

    ("complete"
     (setq PC-meta-flag t)
     ;; New in Emacs 20.0.  You must call this to enable the mode; loading
     ;; complete.el by itself is no longer sufficient.
     (and (fboundp 'partial-completion-mode)
          (partial-completion-mode)))

    ("cperl-mode"
     (load-offer-compile "init-cperl" t))

    ("crypt++"
     (cond ((or (fboundp 'mc-gpg-decrypt-region)
                (load "mc-gpg" t))
            (setq crypt-encryption-type 'gpg)
            (setq crypt-pgp-pub-sub-library 'gpg)
            (crypt-rebuild-tables))
           (t
            (setq crypt-never-ever-decrypt t)))
     (setq crypt-auto-decode-insert           t
           ;; I'd rather be able to recover my data
           crypt-encoded-disable-auto-save   nil
           crypt-encrypted-disable-auto-save nil))

    ("dbfrobs"
     (add-hook 'after-do-inits-hook 'dbfrobs:debug-on-interesting-errors))

    ("dired"
     (load-offer-compile "init-dired" t))

    ("disptime"
     (setq disptime-show-day-and-date-p t)
     (setq disptime-show-time-24hr-p    t)
     (setq disptime-show-load-average-p t)
     (setq disptime-show-mail-p         'count))

    ("eldoc"
     (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
     (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode))

    ("escreen"
     (setq escreen-install-number-mode-format nil)
     (escreen-install)
     (escreen-number-mode 1)
     (add-hook 'escreen-goto-screen-hook
               'escreen-enable-number-mode-if-more-than-one-screen))

    ("eval-expr"
     (eval-expr-install))

    ("ffap"
     (setq ffap-require-prefix t)
     (substitute-key-definition 'find-file 'find-file-at-point global-map)

     (set-alist-slot 'ffap-string-at-point-mode-alist 'makefile-mode
                     '("--:$+<>()@-Z_a-z~*?." "<@" "@>;,!:"))
     (set-alist-slot 'ffap-string-at-point-mode-alist 'file
                     '("--:$+<>@-Z_a-z~*?," "<@" "@>;.,!:")))

    ("fff"
     (load-offer-compile "init-fff" t))

    ("filladapt"
     (progn
       (setq-default filladapt-mode nil)
       (setq filladapt-mode-line-string " FA")
       (add-hook 'text-mode-hook       'turn-on-filladapt-mode)
       (add-hook 'change-log-mode-hook 'turn-off-filladapt-mode)))

    ("flash-paren"
     (flash-paren-mode 1))

    ("fshell"
     (add-hook 'fshell-make-shell-hook 'fshell-inherit-cwd)
     (add-hook 'fshell-after-start-shell-hook
               (lambda ()
                 (when process-connection-type
                   (set-process-window-size
                    (get-buffer-process (current-buffer)) 0 80)))))

    ;;("ftcp"
    ;; (ftcp-open-network-stream-mode 1))

    ("ftelnet"
     (or (fboundp 'otelnet)
         (defalias 'otelnet (symbol-function 'telnet)))
     (defalias 'telnet 'ftelnet))

    ("gnus"
     (load-offer-compile "init-gnus" t))

    ("gnuserv"
     (setq gnuserv-frame t)
     (setenv "P4EDITOR" "gnuedit"))

    ("hscroll"
     (setq hscroll-margin 0
           hscroll-step-percent 95))

    ("imenu"
     (setq imenu-max-item-length "Unlimited")
     (setq imenu-auto-rescan-maxout (* 4 megabyte))
     (setq imenu-always-use-completion-buffer-p 'never))

    ("info"
     (load-offer-compile "init-info" t))

    ("iso-acc"
     (setq iso-accents-mode-string " Acc")
     (set-minor-mode-string 'iso-accents-mode 'iso-accents-mode-string t)
     (iso-acc-install-extra-keys))

    ("jam-mode"
     (load-offer-compile "init-jam" t))

    ("linuxproc"
     (linuxproc-install))

    ("listbuf"
     (defadvice list-buffers (around sinit:listbuf activate)
       (apply 'listbuf (ad-get-args 0)))

     (setq listbuf-ignore-buffer-name-regexp-list
           (delete "^ " listbuf-ignore-buffer-name-regexp-list))

     (and (fboundp 'listbuf-revert-after-execute)
          (add-hook 'post-listbuf-hook 'listbuf-revert-after-execute)))

    ("live-find-file"
     ;; Don't let live-find-file mask any characters.
     (setq live-file-mask (make-keyboard-translate-table 256)))

    ("man"
     (and (facep 'font-lock-variable-name-face)
          (setq Man-overstrike-face 'font-lock-variable-name-face
                Man-underline-face  'font-lock-function-name-face)))

    ("p4"
     (load-offer-compile "init-p4" t))

    ("pb-popup"
     (setq-default pb-popup-mode             t)
     (setq-default pb-popup-ratio            6)
     (setq-default pb-popup-min-height       4)
     (setq-default pb-popup-available-frames nil)

     (make-variable-buffer-local 'pb-popup-mode)

     ;; Defined so as not to override comint-popup if loaded.
     (defun pb-add-comint-popup-hook ()
       (unless (featurep 'comint-popup)
         (make-local-hook 'comint-output-filter-functions)
         (add-hook 'comint-output-filter-functions
                   'pb-popup-current-buffer t t)))

     (mapc (lambda (h)
             (add-hook h 'pb-add-comint-popup-hook t))
       '(gdb-mode-hook
         ftelnet-mode-hook
         rlogin-mode-hook
         shell-mode-hook
         ssh-mode-hook
         telnet-mode-hook)))

    ("perl-mode"
     (setq-default perl-brace-imaginary-offset     0)
     (setq-default perl-brace-offset               0)
     (setq-default perl-continued-brace-offset     0)
     (setq-default perl-continued-statement-offset 2)
     (setq-default perl-indent-level               2)
     (setq-default perl-label-offset               -2)
     (setq-default perl-tab-always-indent          t)
     (setq-default perl-tab-to-comment             nil))

    ("protbuf"
     (setq protect-buffer-verbose nil)
     (set-minor-mode-string 'protect-buffer-from-kill-mode         " PB"  t)
     (set-minor-mode-string 'protect-process-buffer-from-kill-mode " PPB" t))

    ("rlogin"
     (cond
      ((boundp 'rlogin-initially-track-cwd)
       ;; Emacs 19.28 and earlier.
       (setq rlogin-initially-track-cwd nil))
      ((boundp 'rlogin-directory-tracking-mode)
       ;; Emacs 19.29 and later.
       (setq rlogin-directory-tracking-mode 'local)))

     ;; solaris' rlogin spews tty ioctl errors if no pty is present.
     (and (system-configuration-matches-p "solaris2")
          (setq rlogin-process-connection-type t))

     (setq rlogin-explicit-args
           (cond ((system-configuration-matches-p "linux" "lignux" "netbsd")
                  '("-8E"))
                 (t '("-8")))))

    ("rsz-minibuf"
     (eval-after-load-forms-for "rsz-mini"))

    ("rsz-mini"
     (resize-minibuffer-mode 1))

    ("scroll-fix"
     (setq scroll-in-place-replace-original t))

    ("scroll-in-place"
     (setq scroll-in-place t)
     (setq scroll-allow-blank-lines-past-eob t))

    ("sh-script"
     ;; This is the shell I program with by default.
     ;; sh-script.el (as of 19.34) defaults to $SHELL if set, but that's not
     ;; almost never what I use to write portable shell scripts.
     (setq-default sh-shell-file "/bin/sh"))

    ("shell"
     (setq shell-input-autoexpand nil
           shell-popd-regexp      "popd\\|\\]d"
           shell-pushd-regexp     "pushd\\|\\[d"
           shell-cd-regexp        "cd\\|chdir"
           shell-prompt-pattern   (nf-init-make-shell-prompt-pattern)
           shell-delimiter-argument-list nil)

     (when (boundp 'shell-file-name-quote-list)
       ;; Add open/close parens to list of chars to quote when inserting
       ;; filenames.
       (add-list-members 'shell-file-name-quote-list ?\x28 ?\x29))

     (and (boundp 'explicit-bash-args)
          (setq explicit-bash-args
                (delete "--noediting" explicit-bash-args)))

     (define-key shell-mode-map "\M-\C-l" nil))

    ("solar"
     (load-offer-compile "init-solar" t))

    ("ssh"
     (setq ssh-directory-tracking-mode 'local)
     (add-hook 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)"))

    ("text-mode"
     ;; In v19 this is normally bound to ispell-complete-word, but that's
     ;; just too annoying.  The ispell dictionary doesn't have most of the
     ;; words I use, and I'm used to using M-C-i to complete lisp symbols
     ;; anyway.
     (define-key text-mode-map "\M-\C-i" 'lisp-complete-symbol))

    ("time"
     (setq display-time-day-and-date t))

    ("timestamp"
     ;; ISO style is type 6 (see timestamp.el)
     (setq time-stamp-default-type 6))

    ("type-break"
     (type-break-mode t))

    ("w3"
     (load-offer-compile "init-w3" t))

    ("w3m"
     (load-offer-compile "init-w3m" t))

    ("nuke-whitespace"
     (add-hook 'write-file-hooks 'nuke-trailing-whitespace))

    ("win-disp-util"
     (wdu-install-keybindings)
     (add-forms-to-after-load-alist
       '(("scroll-in-place"
          (add-list-members 'scroll-command-groups
            '(wdu-scroll-down-4-lines wdu-scroll-up-4-lines)
            '(wdu-scroll-screen-up    wdu-scroll-screen-down)))
         ("vh-scroll"
          (apply 'add-list-members 'vh-temporary-goal-column-commands
                 wdu-temporary-goal-column-commands)))))

    ("xscheme"
     (load-offer-compile "init-xscheme" t))

    ("zenicb"
     (load-offer-compile "init-zenicb" t))

    ("zenirc"
     (load-offer-compile "init-zenirc" t))))

(add-after-load-libraries
  "complete"
  "crypt++"
  ;; Causes problems in emacs 18.  See stuff.el.
  ;;'(load-offer-compile "disptime")
  '(load-offer-compile "escreen")
  '(load-offer-compile "eval-expr")
  "ffap"
  '(load-offer-compile "hack-locals")
  '(load-offer-compile "listbuf")
  '(load-offer-compile "nuke-whitespace")
  "scroll-in-place"  ; not used after emacs 20
  "show-temp-buffer" ; not used after emacs 20
  '(load-offer-compile "suggbind")
  '(load-offer-compile "what-line" t)
  '(load-offer-compile "win-disp-util"))

;; Load this early because it sets up some byte compilation paths and other
;; hacks I may need for post-initialization details.
(load-offer-compile "all.stuff" t)

;;; init-common.el ends here.
