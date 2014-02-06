;;; init-erc.el --- private initialization for ERC

;; Copyright (C) 2007 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 2007-04-17

;; $Id: init-erc.el,v 1.1 2007/04/18 04:20:18 friedman Exp $

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

(require 'erc)

(setq erc-modules
      '(autojoin
        button
        capab-identify
        irccontrols
        log
        match
        menu
        netsplit
        noncommands
        page
        pcomplete
        readonly
        ring
        scrolltobottom
        stamp
        track
        truncate))

(erc-update-modules)


;;;;;;
;;; Defuns
;;;;;;

(defun init-erc-redefface (face &rest proplist)
  (let ((current (face-attr-construct face))
        (attrs nil))
    (while current
      (setq attrs   (cons (car current) attrs)
            current (cdr (cdr current))))
    (apply 'delete-face-attributes face nil attrs)
    (apply 'override-face-attributes nil proplist)))


;;;;;;
;;; Advice
;;;;;;

(defadvice erc-generate-new-buffer-name (after init-erc:asteriskize activate)
  (save-match-data
    (cond ((string-match "^[#&+]" ad-return-value))
          ((string-match "<[0-9]+>$" ad-return-value)
           (setq ad-return-value
                 (format "*%s*%s"
                         (substring ad-return-value 0 (match-beginning 0))
                         (substring ad-return-value (match-beginning 0)))))
          (t
           (setq ad-return-value
                 (concat "*" ad-return-value "*"))))))


;;;;;;
;;; Mode hooks
;;;;;;

(add-hook 'erc-join-hook (lambda () (protect-buffer-from-kill-mode 1)))



;;;;;;
;;; Variables
;;;;;;

(setq erc-autojoin-channels-alist
      '(("freenode.net"         "#emacs" "#erc")
        ("irc.electricrain.com" "#/dev/gaf" "#/mark/lar")
        ))


(setq erc-interpret-mirc-color             t

      erc-kill-buffer-on-part              nil
      erc-kill-queries-on-quit             nil
      erc-kill-server-buffer-on-quit       nil

      erc-warn-about-blank-lines           t
      erc-send-whitespace-lines            nil
      erc-show-my-nick                     t

      erc-hide-prompt                      t
      erc-prompt                           ""
      erc-command-indicator                "ERC>"

      erc-whowas-on-nosuchnick             t
      erc-public-away-p                    nil
      erc-verbose-server-ping              nil
      erc-away-nickname                    nil
      erc-paranoid                         t
      erc-disable-ctcp-replies             nil
      erc-anonymous-login                  nil

      erc-join-buffer                      'window

      )

(setq erc-header-line-format               "%n on %s (%m,%l) %o"
      erc-mode-line-format                 "%t %a"
      erc-header-line-face-method          t)

(setq erc-hide-timestamps                  nil
      erc-timestamp-format                 "%H:%M "
      erc-insert-timestamp-function        'erc-insert-timestamp-left
      erc-timestamp-only-if-changed-flag   nil)


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(mapc (lambda (l)
        (apply 'init-erc-redefface l))
  '((erc-default-face    :inherit default)
    (erc-direct-msg-face :inherit default)
    (erc-header-line     :inherit font-lock-preprocessor-face)))

;; erc-action-face
;; erc-bold-face
;; erc-button
;; erc-capab-identify-unidentified
;; erc-command-indicator-face
;; erc-current-nick-face
;; erc-dangerous-host-face
;; erc-default-face
;; erc-direct-msg-face
;; erc-error-face
;; erc-fool-face
;; erc-header-line
;; erc-input-face
;; erc-inverse-face
;; erc-keyword-face
;; erc-my-nick-face
;; erc-nick-default-face
;; erc-nick-msg-face
;; erc-notice-face
;; erc-pal-face
;; erc-prompt-face
;; erc-timestamp-face
;; erc-underline-face

(provide 'init-erc)

;;; init-erc.el ends here.
