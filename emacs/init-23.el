;;; init-23.el --- startup for Emacs version 23

;; Copyright (C) 2006, 2010 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-23.el,v 1.20 2011/10/03 06:10:15 friedman Exp $

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
(require 'frame-fns)

;; Set this before loading inits for older emacsen, so that they do not do
;; latin1-specific things.
(set-language-environment "UTF-8")

(unless (featurep 'init-22)
  (load-offer-compile "init-22"))


;;;;;;
;;; Personal variable declarations
;;; These are not variables defined in other packages, but rather they are
;;; declarations for variables I created and use in my initialization files.
;;;;;;

(defvar recenter-top-bottom-scroll-margin 4
  "Margin to use when recentering to top or bottom using `recenter-top-bottom'.
This is implemented using the `sinit:scroll-margin' defadvice on that command.")


;;;;;;
;;; Defuns
;;;;;;

;; ----------
(defun ansi-color-frobnify (&optional prefix)
  (interactive "P")
  (require 'ansi-color)
  (require 'proc-filters)

  (let ((enablep (or (null prefix)
                     (>= (prefix-numeric-value prefix) 0)))
        (ls-colors-string (mapconcat 'identity
                                     '("no=00"
                                       "fi=00"
                                       "di=00;34"
                                       "pi=00;32"
                                       "so=00;32"
                                       "do=00;32"
                                       "bd=00;32"
                                       "cd=00;32"
                                       "ln=00;36"
                                       "or=00;31"
                                       "su=00;31"
                                       "sg=00;31"
                                       "ex=00;33"
                                       "tw=00;34"
                                       "ow=00;34"
                                       "st=00;34")
                                     ":")))
    (setenv "USER_LS_COLORS" ls-colors-string)
    (setenv "LS_COLORS" ls-colors-string)
    (setenv "GNU_LS_OPTIONS" "--color")

    (setq ansi-color-names-vector
          (vector "black"                                         ;; "black"   ; 30 40
                  (face-foreground 'font-lock-warning-face)       ;; "red"     ; 31 41
                  (face-foreground 'font-lock-constant-face)      ;; "green"   ; 32 42
                  (face-foreground 'font-lock-variable-name-face) ;; "yellow"  ; 33 43
                  (face-foreground 'font-lock-function-name-face) ;; "blue"    ; 34 44
                  "magenta"                                       ;; "magenta" ; 35 45
                  (face-foreground 'font-lock-keyword-face)       ;; "cyan"    ; 36 46
                  "white"                                         ;; "white"   ; 37 47
                  ))

    (setq ansi-color-map (ansi-color-make-color-map))

    (add-hook 'proc-filter-shell-output-filters 'proc-filter-misc-ctlseqs)

    (if enablep
        (ansi-color-for-comint-mode-on)
      (ansi-color-for-comint-mode-off))

    (when (interactive-p)
      (message "ansi-color-for-coming-mode %s"
               (if enablep "ON" "OFF")))))

;; ----------
(defadvice recenter-top-bottom (around sinit:scroll-margin activate)
  "Use `recenter-top-bottom-scroll-margin' instead of `scroll-margin'."
  (let ((scroll-margin recenter-top-bottom-scroll-margin))
    ad-do-it))


;;;;;;
;;; Variables
;;;;;;

(setq find-file-confirm-nonexistent-file nil)

(setq line-move-visual                   nil) ; causes excessive gc

(setq mail-user-agent                    'sendmail-user-agent)

;; X selection behavior
(setq x-select-enable-primary            t
      select-active-regions              nil
      yank-pop-change-selection          t)

;; Setting width to nil and height to something unreasonably large makes
;; `display-buffer' in v23 mimic v22-and-earlier behavior.
(setq split-height-threshold             moby-bignum ;  4
      split-width-threshold              nil)        ; 162)


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(minibuffer-depth-indicate-mode 1)
(ansi-color-frobnify)

(for-frame-type ns init-23:no-antialias
  (setq ns-antialias-text nil))


;;;;;;
;;; External libraries to load
;;;;;;

;;(add-forms-to-after-load-alist nil)

;;(add-after-load-libraries nil)

;; We no longer need these libraries
;;(remove-after-load-libraries nil)

(provide 'init-23)

;;; init-23.el ends here.
