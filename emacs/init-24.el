;;; init-24.el --- startup for Emacs version 24

;; Copyright (C) 2010 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-24.el,v 1.8 2011/08/02 18:43:31 friedman Exp $

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


;;;;;;
;;; Defuns
;;;;;;

;; ----------
(defadvice bury-buffer (before sinit:avoid-kill-window activate)
  "Workaround for Emacs 24 and later:

Switch to other-buffer before burying previous buffer.
This is to avoid iconifying frame or deleting the window just because the
buffer is displayed in another window still."
  (when (null (ad-get-arg 0))
    (let ((buf (current-buffer)))
      (switch-to-buffer (other-buffer buf))
      (ad-set-arg 0 buf))))

;; ----------
;; Removed from Emacs 24 2010-10-03 Glenn Morris <rgm@gnu.org>
;; Obsolete, but used in my init files since needed in v20 and earlier.
(defalias 'make-local-hook 'ignore)


;;;;;;
;;; Variables
;;;;;;

;; New completion engine; replaces complete.el
(setq completion-styles '(basic partial-completion emacs22)
      completion-pcm-complete-word-inserts-delimiters t
      completion-pcm--delim-wild-regex "[-_./: ]")


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(unless (featurep 'init-23)
  (load-offer-compile "init-23"))

;; This got changed to isearch-yank-kill, which is already on M-y.
(define-key isearch-mode-map "\C-y" 'isearch-yank-line)


;;;;;;
;;; External libraries to load
;;;;;;

(add-forms-to-after-load-alist
  '(("shell"
     ;; pcomplete adds a space after directory names during completion.
     ;; DO NOT WANT.
     (setq shell-dynamic-complete-functions
           (delq 'pcomplete-completions-at-point
                 shell-dynamic-complete-functions))

     (define-key shell-mode-map (kbd "M-RET") nil))))

;;(add-after-load-libraries nil)

;; We no longer need these libraries
(remove-after-load-libraries
  "complete" ;; obsolete; use completion-styles from now on
  )

(provide 'init-24)

;;; init-24.el ends here.
