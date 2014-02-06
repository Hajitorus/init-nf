;;; init-21.el --- startup for Emacs version 21

;; Copyright (C) 1999, 00, 01, 04, 06, 2007 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-21.el,v 1.69 2011/10/03 06:08:45 friedman Exp $

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

(require 'init)
(require 'face-fns)
(require 'frame-fns)
(require 'list-fns)
(require 'font-lock)

(unless (featurep 'init-20)
  (load-offer-compile "init-20"))

;;;;;;
;;; Defvars
;;;;;;

(defvar rapid-blink-cursor-default-count 3)
(defvar rapid-blink-cursor-default-delay 0.04)

;;;;;;
;;; Defuns
;;;;;;

;; ----------
(defadvice backtrace (around sinit:no-print-limits activate)
  "Do not truncate display of long sequences."
  (let ((print-escape-newlines t)
        (print-level nil)
        (print-length nil)
        (print-circle nil)
        (print-gensym t))
    ad-do-it))

;;----------
(defun init-setup-face (&optional frame &rest l)
  (unless (or (framep frame) (null frame))
    (setq l (cons frame l)
          frame nil))

  (when (null (cdr l)) ;; list of length 1
    (setq l (car l)))

  (cond ((and (nth 1 l)
              (symbolp (nth 1 l)))
         (apply 'override-face-attributes (car l) frame (cdr l)))
        (t
         (let ((order '(:foreground :background :bold
                        :inverse-video :underline))
               (face (car l))
               (attr nil))
           (setq l (cdr l))
           (while order
             (if (car l)
                 (setq attr (cons (car order)
                                  (cons (if (eq :nil (car l)) nil (car l))
                                        attr))))
             (setq order (cdr order)
                   l (cdr l)))
           (apply 'override-face-attributes face frame attr)))))

;; ----------
(defun ifont-lock-setup (&optional frame)
  (interactive)
  (let ((fn (lambda (x) (init-setup-face frame x))))
    (cond
     ((and (display-color-p frame)
           (or (>= (display-color-cells frame) 64)
               (save-match-data (string-match "256color$" (getenv "TERM")))))
      (mapc fn
        '((escape-glyph                 :foreground "lightsteelblue"
                                        :inherit nil)
          (font-lock-builtin-face       "lightsteelblue")
          (font-lock-comment-face       "red")
          (font-lock-constant-face      "aquamarine")
          (font-lock-doc-face           "lightsalmon")
          (font-lock-function-name-face "lightskyblue")
          (font-lock-keyword-face       "cyan")
          (font-lock-preprocessor-face  "cornflowerblue")
          (font-lock-string-face        "lightsalmon")
          (font-lock-type-face          "palegreen")
          (font-lock-variable-name-face "lightgoldenrod")
          (font-lock-warning-face       "pink" nil :nil)
          (highlight                    "black")
          (isearch                      "white" "blue")
          (minibuffer-prompt            "cyan")
          (region                       "white" "blue")
          (secondary-selection          "black" "skyblue4"))))
     ((display-color-p frame)
      ;; These settings are based on my xterm xresources for 8-color
      ;; with bold colors mapped to additional non-bold colors.
      (mapc fn
        '((highlight                    "black")
          (isearch                      "black" "blue")
          (region                       "black")
          (secondary-selection          "black")
          (font-lock-comment-face       "red")
          (font-lock-constant-face      "green"   nil t)
          (font-lock-keyword-face       "cyan"    nil t)
          (font-lock-string-face        "red"     nil t)
          (font-lock-type-face          "green"   nil t)
          (font-lock-variable-name-face "yellow"  nil t)
          (font-lock-warning-face       "magenta" nil t))))
     (t                                 ; mono
      (mapc fn
        '((font-lock-constant-face      :underline     nil)
          (font-lock-comment-face       :inverse-video nil :bold nil)
          (font-lock-function-name-face :inverse-video nil :bold t)
          (font-lock-type-face                             :bold nil)
          (font-lock-warning-face       :inverse-video nil :bold t)))))))

;; ----------
(defadvice other-window (after sinit:blink-cursor activate)
  (and (interactive-p)
       window-system
       cursor-in-non-selected-windows
       (rapid-blink-cursor)))

;; ----------
(defun ioutline-level-by-context ()
  "Determine current outline level by considering level of outer headings.
This is in contrast to simply looking at the length of the string matching
`outline-regexp', which may want to permit arbitrary amounts of leading
whitespace without affecting the level."
  (let ((count 1)
        (outline-level 'outline-level))
    (save-excursion
      (outline-back-to-heading t)
      (while (and (not (bobp))
                  (not (eq (outline-level) 1)))
        (if (fboundp 'outline-up-heading-all)
            ;; emacs 21.3 and earlier
            (outline-up-heading-all 1)
          (outline-up-heading 1 t))
        (unless (bobp)
          (setq count (1+ count))))
      count)))

;; ----------
(defadvice iso-accents-mode (around sinit:use-input-method activate)
  (cond ((interactive-p)
         (call-interactively 'toggle-input-method)
         (message "Called `%s'; using `toggle-input-method' instead."
                  this-command))
        (t
         (setq ad-return-value
               (apply 'toggle-input-method (ad-get-args 0))))))

;; ----------
(defun rapid-blink-cursor (&optional count delay)
  (unless delay (setq delay rapid-blink-cursor-default-delay))
  (let ((x (* 2 (or count rapid-blink-cursor-default-count)))
        (show nil))
    (while (and (not (zerop x))
                     (sit-for delay))
           (setq show (not show))
           (setq x (1- x))
           (internal-show-cursor (selected-window) show))
    (internal-show-cursor (selected-window) t)))

;; ----------
(defadvice tar-summarize-buffer (around sinit:crypt++-hack activate)
  "Prevent Emacs 21's `report-errors' macro from breaking crypt++/tar-mode interaction.

Crypt++ runs from the find-file-hook and used to expect to run even if
set-auto-mode failed on the first pass.  As of Emacs 21.4, this is no
longer the case--see `report-errors' and `normal-mode'.

This advice is a kludge; the right way to fix this problem is probably to
restructure crypt++ to act as a file-name-handler."
  (condition-case err
      ad-do-it
    (error
     (cond ((and (save-match-data (string-match "has size [0-9-]+ - corrupted$"
                                                (nth 1 err)))
                 ;; we're not in the crypt++ phase of resetting the mode
                 (null crypt-buffer-encoding-type)))
           (t (apply 'signal err))))))

;; ----------
;; The tty defaults are `unspecified-bg' and `unspecified-fg'.
(defun toggle-default-tty-bg-fg (&optional prefix frame)
  "Toggle `default' tty face bg/fg between black/white and unspecified.
With positive prefix argument, set to \"black\" and \"white\".
With negative prefix argument, set to \"unspecified-bg\" and
 \"unspecified-fg\", respectively.

Setting to black/white will make it easier to determine whether display is
\"dark\" or \"light\", but will slow down redisplay greatly on slow terminals."
  (interactive "P")
  (unless frame
    (setq frame (selected-frame)))
  (unless (or (frame-parameter frame 'tty)
              (equal (frame-parameter frame 'font) "tty"))
    (error "Frame %s is not a tty frame" frame))
  (cond ((consp prefix)
         (setq prefix 1))
        ((null prefix)
         (setq prefix 0)))
  (let* ((bw '("black" . "white"))
         (us  '("unspecified-bg" . "unspecified-fg"))
         (usp (string= (face-foreground 'default frame)
                       "unspecified-fg"))
         (c (cond ((> prefix 0) bw)
                  ((< prefix 0) us)
                  ((= prefix 0) (if usp bw us)))))
    (override-face-attributes 'default frame
      :background (car c) :foreground (cdr c))))


;;;;;;
;;; Mode hooks
;;;;;;

;; ----------
(add-hook 'python-mode-hook
          (lambda ()
            (make-local-variable 'beginning-of-defun-function)
            (make-local-variable 'end-of-defun-function)
            (setq beginning-of-defun-function 'py-beginning-of-def-or-class)
            (setq end-of-defun-function       'py-end-of-def-or-class)))


;;;;;;
;;; Variables
;;;;;;

;; Setting eval-expression-debug-on-error non-nil means the symbol
;; debug-on-error is shadowed so that you cannot set it.  Since I use a
;; non-nil debug-on-error anyway, setting this to t potentially gets in my
;; way.  (Of course, eval-expr.el doesn't use it anyway.)
(setq eval-expression-debug-on-error         nil
      eval-expression-print-length           nil
      eval-expression-print-level            nil)

(setq print-circle                           t
      print-continuous-numbering             nil
      print-gensym                           t
      print-length                           nil
      print-level                            nil)

(setq resize-mini-windows                    t      ;'grow-only
      max-mini-window-height                 0.25)

;; Disable built-in horizontal scrolling since it is very jittery in
;; process buffers; use vh-scroll instead.
;; (2004-07-09 Is it still bad?  Try it out someday.)
(setq automatic-hscrolling                   nil)

(setq kill-read-only-ok                      t)
(setq isearch-lazy-highlight                 nil)
(setq max-mini-window-height                 1.0)

(setq scalable-fonts-allowed                 t)

;; New in Emacs 21.2
;; If non-nil, save-some-buffers uses this variable.
(setq save-abbrevs                           nil)

(setq-default indicate-empty-lines           nil)
(setq-default show-trailing-whitespace       nil)
(setq overflow-newline-into-fringe           t)
;; As of emacs 22 this is now a buffer-local var
(setq-default cursor-in-non-selected-windows nil)

(setq default-enable-multibyte-characters    t)


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(for-frame-type tty init-21:tty-dark-background
  (setq-default frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list)))

(when (featurep 'multi-tty)
  (make-standard-display-table-frame-local))

(for-frame-type (x nil) display-iso8859-1/palmos
  ;; Do this only for non-utf8 environments
  (unless (save-match-data
            (or (string-match "^UTF-" current-language-environment)
                (first-matching (lambda (sym)
                                  (string-match "^\\(?:mule-\\)?utf-"
                                                (symbol-name sym)))
                  (list default-buffer-file-coding-system
                        default-keyboard-coding-system
                        default-terminal-coding-system))))

    (set-language-environment "Latin-1")
    (setq default-input-method "latin-1-prefix")

    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    (disptable-insert-w32/palmos-8bit-glyphs standard-display-table
                                             nil window-system)))

;; Don't do this automatically for ttys yet; depends on font being used.
(for-frame-type x init-21:balanced-single-quotes
  (display-balanced-single-quotes 1))

(global-font-lock-mode 1)
(setq font-lock-support-mode 'jit-lock-mode)

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(for-frame-type window-system init-21:no-tooltips
  (when (fboundp 'tooltip-mode)
    (tooltip-mode -1)))

;; Emacs 21.3 removed this from minor-mode-alist to put it elsewhere on the
;; mode line.  Put it back, since I define my own mode line format.
(set-minor-mode-string 'vc-mode 'vc-mode t)

;(for-frame-type tty sinit:faster-redisplay
;  (toggle-default-tty-bg-fg -1))

(override-face-attributes 'mode-line
  :box nil
  ;;:box '(:line-width 2 :style released-button)
  :background "black"
  :foreground "white"
  :inverse-video nil)

;; New in emacs 21.2
;; Make mode-line-inactive face exactly like mode-line by removing any
;; other overriding properties.
(when (facep 'mode-line-inactive)
  (apply 'delete-face-attributes 'mode-line-inactive
         (mapcar 'car face-attribute-name-alist))
  (override-face-attributes 'mode-line-inactive :inherit 'mode-line)
  (put 'mode-line-inactive 'face-defface-spec nil))

(override-face-attributes 'fringe :background "black" :foreground "grey70")

(setq apropos-symbol-face 'font-lock-variable-name-face)

;; From Emacs 21.1 NEWS:
;;   "The <delete> function key is now bound to `delete-char' by default."
;;
;(define-key global-map [delete] 'backward-delete-char-untabify)
;;
;; If this key is undefined, then the binding for "C-?" will be used
;; instead, so do that here for the sake of consistency with v20.
(global-unset-key [delete])


;;;;;;
;;; External libraries to load
;;;;;;

(add-forms-to-after-load-alist
  '(("comint"
     ;; Needed to turn off font lock in shell mode in 21.3.
     (remove-hook 'comint-mode-hook 'turn-on-font-lock))

    ("font-lock"
     (ifont-lock-setup) ;; do it to initial frame
     (add-hook 'after-make-frame-functions 'ifont-lock-setup))

    ("hexl"
     (setq hexl-iso          "-iso"
           hexl-options      (format "-hex %s" hexl-iso)
           hexl-follow-ascii t)

     ;; 2005-11-14: at some point during the development cycle, these faces
     ;; were changed to end in `-region' instead of `-area'.  Someday it
     ;; would probably be reasonable to remove the check for the latter.
     (defvar hexl-address-region
       (first-matching 'facep '(hexl-address-region hexl-address-area)))

     (defvar hexl-ascii-region
       (first-matching 'facep '(hexl-ascii-region hexl-ascii-area)))

     (when (facep hexl-address-region)
       (override-face-attributes hexl-address-region
           :inherit   'font-lock-type-face
           :underline nil)

       (defadvice hexl-mode-ruler (after sinit:facify activate)
         "Use hexl address face on ruler also."
         (alter-text-property 0 (length ad-return-value) 'face
                              (lambda (f) (or f hexl-address-region))
                              ad-return-value)))

     (when (facep hexl-ascii-region)
       (override-face-attributes hexl-ascii-region
           :inherit   'font-lock-string-face
           :underline nil))

     (add-hook 'hexl-mode-hook 'turn-on-font-lock))

    ("replace"
     (setq list-matching-lines-face 'font-lock-variable-name-face)
     (setq list-matching-lines-buffer-name-face 'font-lock-constant-face))

    ("outline"
     (setq-default outline-regexp
                   "[ \t]*\\([-*]+\\|([A-Za-z0-9.]+)\\|[A-Za-z0-9.]+[.)] \\)")
     (setq-default outline-level 'ioutline-level-by-context))

    ("shell"
     (setq shell-font-lock-keywords
           '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-comment-face)
             ("^[^ \t\n]+:.*"            . font-lock-string-face)
             ("^\\[Exit [0-9].*\\]"      . font-lock-variable-name-face))))))

(add-after-load-libraries
  '(load-offer-compile "init-lisp" t))

;; We no longer need these libraries
(remove-after-load-libraries
  "rsz-mini"
  ;;"vh-scroll"
  '(load-offer-compile "what-line" t)
  )

(provide 'init-21)

;;; init-21.el ends here.
