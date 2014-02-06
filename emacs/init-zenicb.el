;;; init-zenicb.el --- private initialization for zenicb

;; Copyright (C) 1994, 95, 96, 99, 02, 2004 Noah Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1994-06-23

;; $Id: init-zenicb.el,v 1.24 2008/12/24 18:46:51 friedman Exp $

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

;; These customizations work against zenicb 1.17.  I don't know if they
;; all work correctly with later versions.  I've heard reports that the
;; defadvice of zenicb-display-string does not catch all messages in
;; zenicb 1.31.

;;; Code:

(require 'zenicb)
(require 'pb-popup)

(mapc (lambda (name)
        (let ((debug-on-error t))
          (load (concat "zenicb-" name) t)))
  '(;"add"
    "away"
    "history"
    "nologout"
    "signal"
    "stamp"
    ;"whereis"
    ;"yow"
    ))


(defadvice zenicb-display-string (before sinit:timestamp activate)
  "Insert a timestamp at the front of the message if not already present."
  (save-match-data
    (or (string-match "^[0-9][0-9]:[0-9][0-9] " (ad-get-arg 1))
        (ad-set-arg 1 (concat (substring (current-time-string) 11 16)
                              " "
                              (ad-get-arg 1))))))


(defun zenicb-select-net (interactivep prefix
                          buffer-name server port nick channel istatus)
  (let ((zenicb-initial-channel channel)
        (zenicb-nick            nick)
        (zenicb-server          server)
        (zenicb-port            port)
        (zenicb-initial-status  istatus)
        (buffer-name (cond ((numberp prefix)
                            (format "%s<%d>" buffer-name prefix))
                           (prefix
                            (generate-new-buffer-name buffer-name))
                           (t buffer-name))))
    (cond ((and (get-buffer buffer-name)
                (save-excursion
                  (set-buffer buffer-name)
                  zenicb-active))
           (switch-to-buffer buffer-name))
          (t
           (let ((tmpname nil))
             (unwind-protect
                 (progn
                   (when (get-buffer "*zenicb*")
                     (setq tmpname (generate-new-buffer-name "*zenicb*"))
                     (set-buffer "*zenicb*")
                     (rename-buffer tmpname))
                   (set-buffer (get-buffer-create buffer-name))
                   (rename-buffer "*zenicb*")
                   (zenicb))
               (set-buffer "*zenicb*")
               (rename-buffer buffer-name)
               (when tmpname
                 (set-buffer tmpname)
                 (rename-buffer "*zenicb*"))
               (set-buffer buffer-name)))))))

;;;###autoload
(defun zenicb-krotus (&optional prefix)
  (interactive "P")
  (zenicb-select-net (interactive-p) prefix "#krotus"
                     "evolve.icb.net" 7326 "noah" "krotus" "pil"))

;;;###autoload
(defun zenicb-haze (&optional prefix)
  (interactive "P")
  (zenicb-select-net (interactive-p) prefix "#haze"
                     "evolve.icb.net" 7326 "iNoah" "haze" "pil")
  ;; /m server echoback verbose
  ;; /notify -n heaven
  )

;;;###autoload
(defun zenicb-netbsd (&optional prefix)
  (interactive "P")
  (zenicb-select-net (interactive-p) prefix "*netbsd*"
                     "ftp.netbsd.org" 7326 "noah" "oink" "pil"))


(setq-default zenicb-nick "iNoah")
(make-variable-buffer-local 'zenicb-nick)

(defvar zenicb-pb-popup-public-re "n[0o]ah")

(defun zenicb-pb-popup-proc (proc &rest ignored)
  ;; see process filter reentrancy concerns in pb-popup.el wrt sit-for.
  (and (sit-for 0)
       (pb-popup proc)))

(defun zenicb-pb-popup-public (proc parsedmsg)
  (save-match-data
    (and (string-match zenicb-pb-popup-public-re (nth 1 parsedmsg))
         (sit-for 0)
         (pb-popup proc))))

;; use send-string-to-terminal to get past disable audible bell in
;; termcap (normally I don't want audible bells).
(defun zenicb-emit-beep (proc parsedmsg)
  (send-string-to-terminal "\C-g"))

;; pop up on certain public messages
(zenicb-add-hook 'zenicb-server-b-hook 'zenicb-pb-popup-public 'append)

;; pop up on private messages
(zenicb-add-hook 'zenicb-server-c-hook 'zenicb-pb-popup-proc 'append)
(zenicb-add-hook 'zenicb-server-c-hook 'zenicb-emit-beep     'append)

;; pop up on beep (summons)
(zenicb-add-hook 'zenicb-server-k-hook 'zenicb-pb-popup-proc 'append)
(zenicb-add-hook 'zenicb-server-k-hook 'zenicb-emit-beep     'append)


(defun zenicb-messages ()
  (interactive)
  (occur "noah\\|^[0-9:]+ \\*\\|zenirc\\|zenicb\\|emacs\\|\\.el\\|splode\\|\\[info\\] [^ \n]* wants to annoy you."))

(defvar zenicb-mode-string-trailer
  '(zenicb-nick
    (" " zenicb-nick
     (zenicb-current-victim ("->" zenicb-current-victim)) " ")))

(defun zenicb-set-my-mode-line-format ()
  (interactive)
  (setq mode-line-format (default-value 'mode-line-format))
  (unless (memq 'zenicb-mode-string-trailer global-mode-string-trailer)
    (make-local-variable 'global-mode-string-trailer)
    (setq global-mode-string-trailer
          (nconc '("" zenicb-mode-string-trailer)
                 global-mode-string-trailer)))
  (force-mode-line-update))

(defun zenicb-move-to-eob-on-insert ()
  "Move to the end of the ZenICB buffer on input if before the process mark.
If point is after the process mark, don't move it.  That allows editing.

This function should be put on pre-command-hook."
  (and (memq this-command '(self-insert-command))
       (get-buffer-process (current-buffer))
       (< (point) (process-mark (get-buffer-process (current-buffer))))
       (goto-char (point-max))))

(zenicb-add-hook 'zenicb-mode-hook
                 (lambda ()
                   ;; This is only useful in emacs 19, but harmless in v18.
                   (make-local-variable 'pre-command-hook)
                   (add-hook 'pre-command-hook 'zenicb-move-to-eob-on-insert)

                   ;; Emacs 23 uses utf8 encoding internally, so even if we
                   ;; were to disable multibyte encoding in the buffer the
                   ;; length byte in messages passed to and from the server
                   ;; would be passed unconverted as potentially multibyte
                   ;; values, because we use `format' to send messages to
                   ;; the process.  So make sure that emacs converts to and
                   ;; from unibyte over the network.
                   (when (fboundp 'set-buffer-process-coding-system)
                     (set-buffer-process-coding-system 'iso-8859-1 'iso-8859-1))

                   (zenicb-set-my-mode-line-format)
                   (setq mode-name "ZenICB")
                   (setq scroll-step 1)
                   (setq fill-column
                         (- (window-width (selected-window)) 2))))


;; Experimental anti-boot code

(defvar zenicb-return-group "fnord"
  "*Group to return to.")

(defun zenicb-react-to-boot (proc parsedmsg)
  (and (member (nth 0 parsedmsg) '("Boot" "Idle-Boot"))
       (string-match (format "\\b\\(you\\|%s\\)\\b"
                             (regexp-quote zenicb-nick))
                     (nth 1 parsedmsg))
       (zenicb-send-string proc ?h (concat "g\C-a" zenicb-return-group))))

(defun zenicb-set-return-group (proc parsedcmd)
  (setq zenicb-return-group (cdr parsedcmd)))

(zenicb-add-hook 'zenicb-server-d-hook      'zenicb-react-to-boot)
(zenicb-add-hook 'zenicb-command-group-hook 'zenicb-set-return-group)
(zenicb-add-hook 'zenicb-command-g-hook     'zenicb-set-return-group)


;; Experimental moderator auto-passing.
;; When the current moderator leaves, there is a timeout following by
;; auto-passing to the least-idle user in the group.  If I am given
;; moderator by this method, pass to the next person who says anything
;; publicly; they are the least-idle person.

(defvar zenicb-mod-p nil)
(defvar zenicb-mod-pass-p t)
(make-variable-buffer-local 'zenicb-mod-p)

(defun zenicb-mod-check (proc parsedmsg)
  (save-match-data
    (cond ((or (and (string= (nth 0 parsedmsg) "Pass")
                    (string-match " passed you moderation of group "
                                  (nth 1 parsedmsg)))
               (and (member (nth 0 parsedmsg) '("Pass" "Timeout"))
                    (string-match (format "^%s is now mod"
                                          (regexp-quote zenicb-nick))
                                  (nth 1 parsedmsg))))
           (setq zenicb-mod-p t)
           (zenicb-add-hook 'zenicb-server-b-hook
                            'zenicb-pass-mod-to-next-speaker)))))

(defun zenicb-pass-mod-to-next-speaker (proc parsedmsg)
  (cond ((and zenicb-mod-pass-p zenicb-mod-p)
         (zenicb-send-string proc ?h (concat "pass\C-a" (nth 0 parsedmsg)))
         (zenicb-remove-hook 'zenicb-server-b-hook
                             'zenicb-pass-mod-to-next-speaker)
         (setq zenicb-mod-p nil))))

(add-hook 'zenicb-server-d-hook 'zenicb-mod-check)


(provide 'init-zenicb)

;;; init-zenicb.el ends here.
