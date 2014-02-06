;;; init-agchat.el --- agchat.el customizations

;; Copyright (C) 1996, 1997, 2002 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-agchat.el,v 1.2 2002/08/05 06:07:59 friedman Exp $

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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; My agchat server-side preferences are:
;;
;; @rows 99
;; @cols 1024

;;; Code:

(defun agchat-messages ()
  (interactive)
  (occur "n[o0]ah\\|^\\(^[0-9:]+ \\|\\)P#[0-9]+:C\\|emacs\\|\\.el\\b"))

(defun agchat-noah (&optional newp)
  (interactive "P")
  (agchat nil nil current-prefix-arg)
  (agchat-send-sequence "co noah ?"
                        "/dGarbage collecting..."))

(defun agchat-xemacs (&optional newp)
  (interactive "P")
  (agchat nil nil current-prefix-arg)
  (agchat-send-sequence "co xemacs xemacs"
                        "/dGarbage collecting..."))


(defun iagchat-privmsg-beep (s)
  (and (string-match "\C-g" s)
       ;; use send-string-to-terminal to get past disable audible bell
       ;; in termcap (normally I don't want audible bells).
       (send-string-to-terminal "\C-g")))

(defun iagchat-setup ()
  (and (featurep 'protbuf)
       (protect-buffer-from-kill-mode 1))
  (and (fboundp 'buffer-percentage-mode)
       (buffer-percentage-mode -1))
  (make-local-variable 'pb-popup-min-height)
  (setq pb-popup-min-height 6))


(setq agchat-buffer-name-format "*agchat*")

(add-list-members 'agchat-popup-regexp-list "n[o0]ah" "emacs" "agchat\\.el")

(add-hook 'agchat-output-filter-functions 'agchat-message-timestamp 'append)
(add-hook 'agchat-output-filter-functions 'iagchat-privmsg-beep     'append)

(add-hook 'agchat-mode-hook 'iagchat-setup)

(provide 'init-agchat)

;;; init-agchat.el ends here.
