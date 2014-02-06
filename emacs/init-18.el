;;; init-18.el --- startup for emacs version 18

;; Copyright (C) 1991, 92, 93, 94, 95, 96, 97, 98, 99, 2002 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>>
;; Maintainer: friedman@splode.com

;; $Id: init-18.el,v 1.21 2005/05/24 17:45:02 friedman Exp $

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(require 'backquote)
(require 'advice) ; not standard, but we need it
(require 'emacs18-v19compat)

(cond ((not (featurep 'custom))
       (load "custom" t)
       (load "cust-stub" t)))
(cond ((not (fboundp 'defcustom))
       (defmacro defgroup (&rest args) nil)
       (defmacro defcustom (var value doc &rest args)
         (list 'defvar var value doc))))


;;;;;;
;;; Personal variable declarations
;;; These are not variables defined in other packages, but rather they are
;;; declarations for variables I created and use in my initialization files.
;;;;;;


;;;;;;
;;; Macros
;;;;;;


;;;;;;
;;; Defuns
;;;;;;

;; ----------
;; Stolen from Roland.
(defun make-interactive (symbol &rest interactive-args)
  "Make the function definition of SYMBOL an interactive command.
Remaining arguments, if any, are passed to  interactive  in the function."
  (let ((func (symbol-function symbol))
	interactive)
    (if (commandp func)
	(let ((msg (format "%s is already interactively callable." symbol)))
	  (or (null interactive-args)
	      (y-or-n-p (concat msg "  Continue? "))
	      (error msg))))
    (setq interactive (cons 'interactive interactive-args))
    (fset symbol
	  (if (subrp func)
	      (` (lambda (&rest args)
		   (, (documentation func))
		   (, interactive)
		   (apply (, func) args)))
	    (let ((args (car (cdr func)))
		  doc body)
	      (setq doc (car (cdr (cdr func))))
	      (if (stringp doc)
		  (setq body (cdr (cdr (cdr func))))
		(setq doc nil
		      body (cdr (cdr func))))
	      (` (lambda (, args)
		   (, doc)		;might be nil
		   (, interactive)
		   (,@ body))))))))

;; ----------
(defun remap-minibuffer-keys ()
  "User defined function.  Remaps keys in the various minubuffer keymaps"
  (mapc (lambda (key-defn-list)
          (mapc (lambda (keymap)
                  (define-key keymap (car key-defn-list)
                    (car (cdr key-defn-list))))
            (list minibuffer-local-map
                  minibuffer-local-completion-map
                  minibuffer-local-must-match-map
                  minibuffer-local-ns-map)))
    '(("\C-w"     minibuffer-backward-kill-word)
      ("\e\C-w"   minibuffer-kill-region))))


;; This isn't where this should be done, but undefine `C-x n' since it
;; init-common.el assumes it is a keymap (the narrowing commands are on it)
(or (keymapp (key-binding "\C-xn"))
    (define-key ctl-x-map "n" nil))

;; init-common.el (or the libraries it loads) doesn't work in v18 anymore
;;(setq load-offer-compile-default-action 'load-compiled)
;;(load-offer-compile "init-common")


;;;;;;
;;; Mode hooks  (* = not standard hook supplied by emacs or package)
;;;
;;;;;;


;;;;;;
;;; Variables
;;;;;;

(setq
   inhibit-local-variables 'query      ; `enable-local-variables' in v19.
   meta-flag               t           ; Use set-input-mode in v19.
   undo-threshold          megabyte    ; renamed to `undo-limit' in v19.
   undo-high-threshold     moby-bignum ; renamed to `undo-strong-limit' in v19.
)

;; Frob flow control
(set-input-mode t nil)

(make-interactive 'erase-buffer)
(make-interactive 'blink-matching-open)

;; I decided not to mess with the Emacs*reverseVideo attribute in 19, and I
;; don't want to set it for 18 either.
(cond
 ((eq window-system 'x)
  (and (string= "white" (x-get-background-color))
       (x-flip-color))))


;;;;;;
;;; External libraries to load
;;;;;;

;; Add forms to after-load-alist for various libraries.
;; Doing this instead of just loading once and modifying means that these
;; modifications will happen every time the library is loaded.
(add-forms-to-after-load-alist
  '(("rsz-minibuf"
     (remap-minibuffer-keys)
     (setq minibuffer-max-window-height nil))))

(add-after-load-libraries
  "killbuf"               ; v19-compatible kill-buffer (runs various hooks)
  "next-line"             ; emacs 19 uses `next-line-add-newlines' instead.
  "rsz-minibuf"
  "smart-ff"              ; emacs 19 has this built in.
  "vc-hooks"              ; autoloaded in 19
  "vc"                    ; autoloaded in 19
  )

;;; init-18.el ends here
