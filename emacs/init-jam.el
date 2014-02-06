;;; init-jam.el --- initialization for jam-mode

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 2010-10-07
;; Public domain

;; $Id: init-jam.el,v 1.2 2010/11/12 00:39:46 friedman Exp $

;;; Commentary:
;;; Code:

(defvar jam-imenu-generic-expression
  `((nil      "^\\s-*\\(?:rule\\|actions\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
    ("rule"   "^\\s-*\\(?:rule\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"           1)
    ("action" "^\\s-*\\(?:actions\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"        1)))

;;;;;;
;;; Defuns
;;;;;;

(defun jam-beginning-of-defun (&optional arg)
  (re-search-backward "^\\s-*\\b\\(?:rule\\|actions\\)\\s-+"))

(defun jam-mode-isetup ()
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression jam-imenu-generic-expression)
  (imenu-add-menubar-index)

  (make-local-variable 'beginning-of-defun-function)
  (setq beginning-of-defun-function 'jam-beginning-of-defun)

  (setq tab-width 4))

;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(add-hook 'jam-mode-hook 'jam-mode-isetup)

(provide 'init-jam)

;;; init-jam.el ends here
