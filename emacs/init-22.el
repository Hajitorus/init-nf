;;; init-22.el --- startup for Emacs version 22

;; Copyright (C) 2005, 2006 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-22.el,v 1.22 2011/12/05 21:52:27 friedman Exp $

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
(require 'cl)  ; needed for flet

(unless (featurep 'init-21)
  (load-offer-compile "init-21"))


;;;;;;
;;; Macros
;;;;;;

(defmacro iquail-annotate-input-method (method &rest bindings)
  "Add new bindings to quail-based input methods"
  (when (symbolp method)
    (setq method (symbol-name method)))
  `(progn
     (require 'quail)
     (unless (assoc ,method quail-package-alist)
       (load ,(format "quail/%s" method)))
     (let ((quail-orig (quail-name)))
       (unwind-protect
           (progn
             (quail-select-package ,method)
             (quail-define-rules ((append t)) ,@bindings))
         (when quail-orig
           (quail-select-package quail-orig))))))

(lisp-indent-like 'iquail-annotate-input-method 'when)


;;;;;;
;;; Defuns
;;;;;;

(defadvice what-cursor-position (around sinit:current-line activate)
  "Append current line number to echo area output."
  (flet ((message (&rest args) (apply 'format args)))
    ad-do-it)
  (message "%s line=%d" ad-return-value (line-number-at-pos)))


;;;;;;
;;; Variables
;;;;;;

;; formerly comint-use-prompt-regexp-instead-of-fields
(setq comint-use-prompt-regexp nil)

;; This variable was introduced in v22 (2003-03-02).
;; Don't let it change my fill-column for docstrings!
(setq emacs-lisp-docstring-fill-column nil)

;; this variable is set to `t' in the cvs builds for debugging purposes.
;; When set, it can leak memory and cause other problems.
(when (boundp 'undo-ask-before-discard)
  (setq undo-ask-before-discard nil)
  (add-list-members 'warning-suppress-types '(undo discard-info)))

;; The default for this changed in v24 to nil.
(setq mouse-drag-copy-region t)


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

;; On startup, the *scratch* buffer has an autosave but no actual file name
;; associated with it.  This is so that if you try to kill emacs, it will
;; prompt you to save whatever you might have in that buffer.
;; Well, it's a scratch buffer for chrissakes.  No, I don't want to save it.
;; And anyway what about when this buffer is killed prior to exiting emacs,
;; or killed and recreated?  Consistency, people, consistency.
;; Have to do this after startup has completed.
(add-hook 'after-command-line-1-hook
          (lambda ()
            (if (get-buffer "*scratch*")
                (with-current-buffer "*scratch*"
                  (auto-save-mode -1)
                  (setq buffer-offer-save nil)))))


;;;;;;
;;; External libraries to load
;;;;;;

(add-forms-to-after-load-alist
  '((compile
     (mapc (lambda (x)
             (face-spec-reset-face (car x))
             (set-face-attribute (car x) nil :inherit (cdr x)))
       '((compilation-warning . font-lock-warning-face)
         (compilation-info    . font-lock-doc-face))))

    (make-mode
     (mapc (lambda (x)
             (face-spec-reset-face (car x))
             (set-face-attribute (car x) nil :inherit (cdr x)))
       '((makefile-space       . trailing-whitespace)
         (makefile-targets     . font-lock-function-name-face)
         (makefile-shell       . unspecified)
         (makefile-makepp-perl . font-lock-type-face))))

    (rfc1345 ; use symbol, since quail is weird
     ;; Put all the common currency symbols on a "&$" prefix.
     (let ((bindings (mapcar (lambda (p)
                               (list (car p) (decode-char 'ucs (cadr p))))
                             '(("&$c" #x00a2) ; cent
                               ("&$p" #x00a3) ; british pound
                               ("&$u" #x00a4) ; generic currency sign
                               ("&$y" #x00a5) ; yen
                               ("&$e" #x20ac) ; euro
                               ))))
       ;; macro application ftl
       (eval `(iquail-annotate-input-method rfc1345 ,@bindings))))

    (server
     (setq server-use-tcp            t
           server-name               "gnuedit"
           server-host               "0.0.0.0"
           server-port               (+ 31337 (user-uid))
           server-raise-frame        t
           server-kill-new-buffers   nil

           server-log                nil
           iserver-log-time-format   "%Y-%m-%d %H:%M:%S.%3N%z"
           server-log-time-function  (lambda (&optional time)
                                       (format-time-string iserver-log-time-format time)))

     (add-hook 'server-done-hook
               (lambda ()
                 (unless server-kill-new-buffers (bury-buffer)))))
    ))

;;(add-after-load-libraries nil)
(when (featurep 'multi-tty)
  (add-after-load-libraries
    '(load-offer-compile "all.mtty" t)))

;; We no longer need these libraries
;;(remove-after-load-libraries nil)

(provide 'init-22)

;;; init-22.el ends here.
