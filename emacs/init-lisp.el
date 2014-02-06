;;; init-lisp.el --- lisp-mode startup

;; Copyright (C) 2006, 2007 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-lisp.el,v 1.1 2007/08/09 08:11:44 friedman Exp $

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
;; Inc.; 51 Franklin Street, Fifth Floor;Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(require 'font-lock)

;;;;;;
;;; Defvars
;;;;;;

(defconst init-lisp-font-lock-keywords
  (nconc (list (list (concat
                       "(" (regexp-opt
                            '("init-eval-and-compile-when"
                              "init-eval-and-compile-unless"
                              "for-frame-type"
                              "for-window-system-frames"
                              "for-tty-frames"
                              "save-frame-excursion"
                              "defdeep-copy") t)
                       "\\>")
                     . (1 font-lock-keyword-face))

               (list (concat
                      "(" (regexp-opt
                           '("defun-compile"
                             "defun-undefined"
                             "defalias-undefined") t)
                      "[ \t']*\\(\\sw+\\)?")
                     '(1 font-lock-keyword-face)
                     '(2 font-lock-function-name-face))

               (list (concat
                      "(" (regexp-opt
                           '("require-offer-compile") t)
                      "[ \t']*\\(\\sw+\\)?")
                     '(1 font-lock-keyword-face)
                     '(2 font-lock-constant-face nil t)))
         lisp-font-lock-keywords-2))

;;;;;;
;;; Defuns
;;;;;;

;; ----------
(defun init-enhance-elisp-mode-font-lock ()
  (or (memq 'init-lisp-font-lock-keywords (car font-lock-defaults))
      ;; Can't just setcar my keywords onto the defaults because the damned
      ;; thing is dumped into emacs and is read-only.
      (setq font-lock-defaults
            (cons (append (car font-lock-defaults)
                          '(init-lisp-font-lock-keywords))
                  (cdr font-lock-defaults)))))

;;;;;;
;;; Mode hooks
;;;;;;

(add-hook 'emacs-lisp-mode-hook 'init-enhance-elisp-mode-font-lock)

(provide 'init-lisp)

;;; init-lisp.el ends here.
