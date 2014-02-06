;;; init-zenirc.el --- private initialization for zenirc 3.0

;; Copyright (C) 1994, 95, 96, 97, 99, 00, 04, 07, 2008 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1994-06-23

;; $Id: init-zenirc.el,v 1.130 2010/06/15 17:50:44 friedman Exp $

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

(require 'zenirc)
(require 'list-fns)

(mapc (lambda (name)
        (let ((debug-on-error t))
          (load (concat "zenirc-" name) t)))
  '("8ball"
    "away"
    "complete"
    "ctcp-invite"
    "dcc"
    "fill"
    "format"
    "history"
    "ignore"
    "iwantop"
    "meditate"
    "msgstamp"
    "netsplit"
    "notify"
    "oink"
    "shop"
    "stamp"
    "popup"
    "yow"
    "yow-filter"))


;;; Misc functions.

(defun zenirc-list-commands ()
  (interactive)
  (save-match-data
    (let ((names
           (sort
            (mapcar (lambda (s)
                      (string-match "^zenirc-command-\\(.*\\)-hook$" s)
                      (substring s (match-beginning 1) (match-end 1)))
                    (all-completions "zenirc-command-" obarray
                                     (lambda (s)
                                       (string-match "-hook$"
                                                     (symbol-name s)))))
            'string-lessp)))
      (cond ((interactive-p)
             (with-output-to-temp-buffer "*ZenIRC Commands*"
               (while names
                 (princ (car names))
                 (princ "\n")
                 (setq names (cdr names))))))
      names)))


;;; Signal handling for irc messages

(defvar zenirc-private-signal-regexp "^\0$")
(defvar zenirc-vanity-signal-regexp "PRIVMSG[^:]+:.*[Nn][Oo0*][Aa][Hh]\\b")
(defvar zenirc-public-signal-regexp "PRIVMSG")

(defvar zenirc-signal-symbols
  '(zenirc-private-signal-regexp
    zenirc-vanity-signal-regexp
    zenirc-public-signal-regexp))

(make-variable-buffer-local 'zenirc-private-signal-regexp)
(make-variable-buffer-local 'zenirc-signal-list)

(defun zenirc-public-signal-mode (&optional prefix)
  (interactive "P")
  (let* ((sym 'zenirc-public-signal-regexp)
        (onp (memq sym zenirc-signal-symbols))
        (buflist (buffer-list)))
    (cond
     ((null prefix)
      (if onp
          (setq zenirc-signal-symbols
                (delq sym zenirc-signal-symbols))
        (setq zenirc-signal-symbols
              (append zenirc-signal-symbols (list sym)))))
     ((>= prefix 0)
      (or onp
          (setq zenirc-signal-symbols
                (append zenirc-signal-symbols (list sym)))))
     (t
      (or onp
          (setq zenirc-signal-symbols
                (delq sym zenirc-signal-symbols)))))
    (save-excursion
      (while buflist
        (set-buffer (car buflist))
        (setq buflist (cdr buflist))
        (and (eq major-mode 'zenirc-mode)
             (zenirc-reset-my-signal-list))))
    (and (interactive-p)
         (if (memq sym zenirc-signal-symbols)
             (message "zenirc public signals enabled")
           (message "zenirc public signals disabled")))))

(defun zenirc-reset-private-signal-regexp (&optional to)
  (setq to (or to zenirc-nick (user-login-name)))
  (setq zenirc-private-signal-regexp
        (concat "\\(PRIVMSG\\|NOTICE\\|INVITE\\) +"
                (regexp-quote to)
                "\\b")))

(defun zenirc-reset-my-signal-list (&optional nick)
  (interactive)
  (zenirc-reset-private-signal-regexp nick)
  (setq zenirc-signal-list (mapcar 'symbol-value zenirc-signal-symbols)))

(defun zenirc-NICK-reset-my-signal-list (proc msg-vector)
  (let ((from (zenirc-extract-nick (aref msg-vector 1)))
        (to (aref msg-vector 2)))
    (and (member (zenirc-downcase-name zenirc-nick)
                 (list (zenirc-downcase-name to)
                       (zenirc-downcase-name from)))
         (zenirc-reset-my-signal-list to))))

(zenirc-add-hook 'zenirc-server-NICK-hook
                 'zenirc-NICK-reset-my-signal-list 'append)

(defun zenirc-privmsg-beep (proc parsedmsg)
  (and (zenirc-names-equal-p (aref parsedmsg 2) zenirc-nick)
       ;; use send-string-to-terminal to get past disable audible bell in
       ;; termcap (normally I don't want audible bells).
       (send-string-to-terminal "\C-g")))

(zenirc-add-hook 'zenirc-server-PRIVMSG-hook 'zenirc-privmsg-beep)
;(zenirc-remove-hook 'zenirc-server-PRIVMSG-hook 'zenirc-privmsg-beep)


;; ...clients that pepper their text with random control characters...
;; maybe someday I'll make zenirc support underline, highlight, etc. but
;; then again, probably not.

(defvar init-zenirc-nuke-ctl-chars-regexp
  (let ((c (make-string 32 0))
        (i 0))
    (while (< i (length c))
      (cond ((= i 9))  ;; don't molest TAB,CR,LFD
            ((= i 10))
            ((= i 13))
            ((aset c i i)))
      (setq i (1+ i)))
    (concat "[" c "]+")))

(defun init-zenirc-nuke-ctl-chars (beg end)
  "Remove non-printing ctrl chars from region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (save-match-data
        (narrow-to-region beg end)
        (goto-char beg)
        (while (re-search-forward init-zenirc-nuke-ctl-chars-regexp nil t)
          (delete-region (match-beginning 0) (match-end 0)))))))

(defun init-zenirc-message-nuke-ctl-chars (proc sym string)
  (and proc
       (init-zenirc-nuke-ctl-chars (point-min) (point-max))))

(zenirc-add-hook 'zenirc-message-hook 'init-zenirc-message-nuke-ctl-chars)


(defvar zenirc-mode-string-trailer
  '(zenirc-nick
    (" " zenirc-nick
     (zenirc-current-victim ("->" zenirc-current-victim)) " ")))

(defun zenirc-set-my-mode-line-format ()
  (interactive)
  (setq mode-line-process '(":%s")
        mode-line-format (default-value 'mode-line-format))
  (unless (memq 'zenirc-mode-string-trailer global-mode-string-trailer)
    (make-local-variable 'global-mode-string-trailer)
    (setq global-mode-string-trailer
          (nconc '("" zenirc-mode-string-trailer)
                 global-mode-string-trailer)))
  (force-mode-line-update))

(defun zenirc-move-to-eob-on-insert ()
  "Move to the end of the ZenIRC buffer on input if before the process mark.
If point is after the process mark, don't move it.  That allows editing.

This function should be put on pre-command-hook."
  (and (memq this-command '(self-insert-command))
       (get-buffer-process (current-buffer))
       (< (point) zenirc-process-mark)
       (goto-char (point-max))))


(defun zenirc-irclog-fixup (&optional beg end)
  (interactive "r")
  (unless beg
    (setq beg (region-beginning)
          end (region-end)))
  (save-excursion
    (save-restriction
      (save-match-data
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward
                "^\\(.*\\)\n\\([0-9:]+\\) (sent to \\(.*?\\))"
                nil t)
          (if (and (match-string 3)
                   (memq (aref (match-string 3) 0) '(?# ?+ ?&)))
              (replace-match "\\2 <iNoah> \\1")
            (let ((text   (match-string 1))
                  (tstamp (match-string 2))
                  (to     (match-string 3)))
              (save-match-data
                (when (string-match (format "^/[^ \t\n]+[ \t]+%s[ \t]+" to)
                                    (regexp-quote text))
                  (setq text (substring text (match-end 0)))))
              (replace-match (concat "\\2 *->\\3* "))
              (insert text))))))))


;; regexps in sublists with car = ("#chan1" ...) are only added
;; when the current channel is one of those channels.
;; regexps in sublists with car = (! "#chan1" ...) are excluded
;; only when the current channel is one of those channels.
(defvar zenirc-messages-re-list
  '(((! "#emacs") "emacs")
    ((! "#emacs") "\\.elc?\\b")
    ((! "#emacs" "#lisp") "lisp")
    ((! "#emacs") "n[.o0*]ah")
    (("#emacs") "n[.o0*]ah\\b")         ; avoid noahslater
    "fr[ie]+dman"
    "^\\*"
    "^\\[?[0-9:]+\\]? +\\*"
    "^\\[dcc\\]"
    "zenirc"
    "zenicb"
    "splode"))

(defvar zenirc-messages-re nil)
(make-variable-buffer-local 'zenirc-messages-re)

(defun zenirc-messages-re ()
  (let ((re-list nil))
    (mapc (lambda (elt &optional skip)
            (cond ((consp elt)
                   (setq skip t)
                   (catch 'done
                     (mapc (lambda (chan)
                             (cond ((eq '! chan)
                                    (setq skip nil))
                                   ((string= chan zenirc-current-victim)
                                    (setq skip (not skip))
                                    (throw 'done skip))))
                       (car elt)))
                   (or skip
                       (setq re-list (cons (cadr elt) re-list))))
                  (t (setq re-list (cons elt re-list)))))
      zenirc-messages-re-list)
    (mapconcat 'identity re-list "\\|")))

(defun zenirc-messages ()
  (interactive)
  (or zenirc-messages-re
      (setq zenirc-messages-re (zenirc-messages-re)))
  (occur zenirc-messages-re))


(defvar zenirc-server-alist-efnet
  '(
    ;;("irc-e.irc.lightning.net")         ; identd nazi
    ;;("irc.concentric.net")
    ;;("irc-w.primenet.com")              ; identd nazi
    ;;("irc.mindspring.com")
    ;;("irc.emory.edu")                 ; gone
    ;;("irc.mcs.net")
    ;;("irc.west.gblx.net")
    ("irc.lightning.net")
    ("irc.estmtl.ca")
    ("irc.magic.ca")
    ("irc.fasti.net")
    ;;("irc.pacbell.net")               ; identd nazi
    ;;("irc.mcs.net")                   ; identd nazi
    ;;("irc.idle.net")                  ; identd nazi
    ;;("irc.prison.net")                ; has a short idle timeout
    ;;("irc.blackened.com")             ; gone
    ;;("irc.cerf.net")                  ; said "you're not authorized"
    ;;("irc.primenet.com")              ; said "you're not authorized"
    ;;("irc2.lagged.org")               ; said "you're not authorized"
    ("irc.ionet.net")
    ("irc.mit.edu")
    ("irc-2.mit.edu")
    ("irc.mit.edu")
    ("irc.warped.net")                  ; is this really efnet?
    ))

(defvar zenirc-server-alist-gafnet
  '(("electricrain.com" 6667)
    ("popskull.net" 6667)))

(defvar zenirc-server-alist-mednet
  '(("petting-zoo.net"     7979) ;; gkm
    ("amonduul.ecn.ou.edu" 7979) ;; rmtodd
    ("cedar.plexus.com"    7979) ;; scottr
    ("gratuitous.com"      7979) ;; wisner
    ("caligula.anu.edu.au" 7979)
    ("gaia.eterna.com.au"  7979) ;; mrg
    ))

(defvar zenirc-server-alist-moznet
  '(("irc.mozilla.org")
    ("linux.mit.edu")
    ("irc.cabi.net")
    ("beast.blackdown.org")
    ))

(defvar zenirc-server-alist-tcnet
  '(("irc.gigo.com")
    ("irc.anybrowser.org")
    ("bsdnet.in-addr.com")
    ("irc.themusic.net")
    ("irc.photon.com")
    ("windborne.net")))

(defun zenirc-select-net (interactivep prefix alist
                          &optional buffer fullname nick login commands)
  (let ((zenirc-server-alist (or alist zenirc-server-alist))
        (zenirc-buffer-name (or buffer zenirc-buffer-name))
        (zenirc-user-full-name-default (or fullname
                                           zenirc-user-full-name-default))
        (zenirc-nick-default (or nick zenirc-nick-default))
        (zenirc-user-login-name-default (or login
                                            zenirc-user-login-name-default)))
    (if interactivep
        (call-interactively 'zenirc)
      (zenirc prefix))

    (when (featurep 'protbuf)
      (protect-process-buffer-from-kill-mode -1)
      (protect-buffer-from-kill-mode 1))

    ;; Some irc servers won't let us send additional commands until they
    ;; acknowledge our registration and send the first logged-in server
    ;; response (001).  So push these commands onto the 001 hook, to be
    ;; removed after they are run.
    (when commands
      (let ((fn (make-symbol "<zenirc-select-net commands>")))
        (fset fn `(lambda (proc parsedmsg)
                    ;; Make sure they are only run for the correct process,
                    ;; in case there are race conditions.
                    (when (eq proc ,(get-buffer-process (current-buffer)))
                      (unwind-protect
                          (process-send-string proc
                            (concat ,(mapconcat 'identity commands "\n") "\n"))
                        (zenirc-remove-hook 'zenirc-server-001-hook ',fn)))))
        (zenirc-add-hook 'zenirc-server-001-hook fn t)))))

;;###autoload
(defun zenirc-android (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     '(("irc.freenode.net" 6667)) "#android"
                     nil "noahdroid" nil
                     (list
                      (string<->vector [  80  82  73  86  77  83  71  32
                                         110 105  99 107 115 101 114 118
                                          32  58 105 100 101 110 116 105
                                         102 121  32 111 105 110 107])
                      "JOIN #android-dev"
                      "JOIN #android"
                      "JOIN #android-root"
                      "JOIN #cyanogenmod")))

;;;###autoload
(defun zenirc-cantina (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-moznet "#cantina"
                     nil "noah" nil
                     '("JOIN #cantina tequila")))

;;;###autoload
(defun zenirc-dnalounge (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     '(("irc.dnalounge.com" 6667)) "*zenirc-dnalounge*"
                     nil nil nil
                     '("JOIN #dnalounge")))

;;;###autoload
(defun zenirc-efnet (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-efnet "*efnet*"
                     "encased in the lining of a pure pork sausage"
                     "iNoah" "yow"))

;; Having problems connecting to a lot of efnet servers; let me choose.
;;;###autoload
(defun zenirc-efnet-alternate (server &optional prefix)
  (interactive (list (completing-read "efnet server: "
                                      zenirc-server-alist-efnet)
                     current-prefix-arg))
  (let ((zenirc-server-alist-efnet
         (memq (assoc server zenirc-server-alist-efnet)
               zenirc-server-alist-efnet)))
    (zenirc-efnet prefix)))

;;;###autoload
(defun zenirc-emacs (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     '(("irc.freenode.net" 6667)) "#emacs"
                     nil "iNoah" nil
                     '("JOIN #emacs")))

;;;###autoload
(defun zenirc-gafnet (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-gafnet "#/dev/gaf"
                     "encased in the lining of a pure pork sausage"
                     "iNoah" "yow"
                     '("MODE iNoah -x"
                       "JOIN #/dev/gaf knio")))

;;;###autoload
(defun zenirc-chat (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-gafnet "#chat"
                     "encased in the lining of a pure pork sausage"
                     "_noah" "yow"
                     '("MODE _noah -x"
                       "JOIN #chat")))

;;;###autoload
(defun zenirc-krotus (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-gafnet "#krotus2"
                     "encased in the lining of a pure pork sausage"
                     "kNoah" "yow"
                     '("MODE kNoah -x"
                       "JOIN #krotus")))

;;;###autoload
(defun zenirc-marklar (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-gafnet "#/mark/lar"
                     "21st century schizoid man"
                     "noah" nil
                     '("MODE noah -x"
                       "JOIN #/mark/lar")))

;;;###autoload
(defun zenirc-mednet (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-mednet "#meditation"
                     "encased in the lining of a pure pork sausage"
                     nil nil
                     '("JOIN #root"
                       "JOIN #meditation")))

;;;###autoload
(defun zenirc-mozilla (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-moznet "#mozilla"
                     nil "iNoah" nil
                     '("JOIN #mozilla")))

;;;###autoload
(defun zenirc-sfrivethead (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     '(("irc.freenode.net" 6667)) "#sfrivethead"
                     "21st century schizoid man"
                     nil "mini-waffles" nil nil))
;;;###autoload
(defun zenirc-splodenet (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     '(("localhost")
                       ("irc.prv.splode.com"))
                     "#splode"))

;;;###autoload
(defun zenirc-tcnet (&optional prefix)
  (interactive "P")
  (zenirc-select-net (interactive-p) prefix
                     zenirc-server-alist-tcnet "#tc"
                     nil nil nil
                     '("JOIN #tc")))

;;;###autoload
(defun zenirc-bitlbee (&optional prefix bufname nick pass)
  (interactive "P")
  (unless nick (setq nick "noah"))
  (zenirc-select-net (interactive-p) prefix '(("localhost" 6669))
                     (or bufname "#bitlbee")
                     nick nick nick
                     (if pass
                         (list (format "identify %s" pass))))
  (make-local-variable 'zenirc-notify-list)
  (setq zenirc-notify-list nil)

  (make-local-copied-variables 'zenirc-message-hook)
  ;(zenirc-add-hook 'zenirc-message-hook 'zenirc-bitlbee-nuke-markup)
  )

(defun zenirc-bitlbee-nuke-markup (&rest ignore)
  (goto-char (point-min))
  (save-match-data
    (while (re-search-forward "\\(\\[#[0-9]+m\\)?</?\\(font\\|fade\\|body\\|html\\)[^>]*>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun zenirc-bitlbee-cleanup ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (regexp-opt
             '(" - Couldn't log in: "
               " - Error: Disconnected."
               " - Error: Error while reading from server"
               " - Error: Stream error: "
               " - Logging in: "
               " - Reconnecting in "
               " - Signing off.."
               " - Warning: Received incomplete IQ-get packet"
               " has joined &bitlbee"
               " stopped wasting time:"
               "[info] netjoin: localhost."
               "[info] netsplit: localhost."
               )) nil t)
      (delete-region (progn (beginning-of-line)   (point))
                     (progn (beginning-of-line 2) (point))))))

;;;###autoload
(defun zenirc-yahoo-im (&optional prefix)
  (interactive "P")
  (zenirc-bitlbee prefix "#yahoo" "noah_yahoo"))

;;;###autoload
(defun zenirc-aim (&optional prefix)
  (interactive "P")
  (zenirc-bitlbee prefix "#aim" "noah_aim"))

;;;###autoload
(defun zenirc-google (&optional prefix)
  (interactive "P")
  (zenirc-bitlbee prefix "#google" "noah_google"))

(defun zenirc-livejournal (&optional prefix)
  (interactive "P")
  (zenirc-bitlbee prefix "#livejournal" "noah_livejournal"))

;;;###autoload
(defun zenirc-msn (&optional prefix)
  (interactive "P")
  (zenirc-bitlbee prefix "#msn" "noah_msn"))

;;;###autoload
(defun zenirc-standard-splode (&optional prefix)
  (interactive "P")
  (zenirc-public-signal-mode -1)
  (mapc (lambda (fn)
          (if (interactive-p)
              (call-interactively fn)
            (funcall fn prefix)))
    '(zenirc-gafnet
      zenirc-chat
      ;;zenirc-krotus
      ;;zenirc-marklar
      zenirc-mednet
      ;;zenirc-mozilla
      ;;zenirc-sfrivethead
      ;;zenirc-splodenet
      zenirc-tcnet
      ;;zenirc-emacs
      ;;zenirc-cantina

      zenirc-aim
      zenirc-livejournal
      ;;zenirc-msn
      zenirc-yahoo-im
      zenirc-google

      zenicb-krotus
      zenicb
      )))


(zenirc-add-hook 'zenirc-mode-hook 'zenirc-set-my-mode-line-format)
;(zenirc-add-hook 'zenirc-mode-hook 'turn-on-auto-fill)
(zenirc-add-hook 'zenirc-mode-hook 'turn-off-auto-fill)
(zenirc-add-hook 'zenirc-mode-hook
                 (lambda ()
                   ;; This is only useful in emacs 19, but harmless in v18.
                   (make-local-variable 'pre-command-hook)
                   (add-hook 'pre-command-hook 'zenirc-move-to-eob-on-insert)
                   (setq scroll-step 1)
                   (setq fill-column
                         (- (window-width (selected-window)) 2))))

(zenirc-add-hook 'zenirc-connect-hook
                 (lambda (proc)
                   (zenirc-reset-my-signal-list zenirc-nick)
                   (process-kill-without-query proc)
                   (when (featurep 'mule)
                     (set-buffer-file-coding-system 'utf-8)
                     (set-buffer-process-coding-system 'utf-8 'utf-8))))

;; Make these direct aliases for privmsg
;; It's somewhat egregious that these are separate.
(setq zenirc-command-msg-hook
      (lambda (&rest args)
        (apply 'zenirc-run-hook 'zenirc-command-privmsg-hook args)))
(setq zenirc-command-m-hook zenirc-command-msg-hook)

;; Make /default an alias for /query, for compatibility with rocker's irc.el
(setq zenirc-command-default-hook
      (lambda (&rest args)
        (apply 'zenirc-run-hook 'zenirc-command-query-hook args)))

(zenirc-remove-hook 'zenirc-signal-hook 'zenirc-signal)

(zenirc-add-hook 'zenirc-dcc-chat-filter-hook 'zenirc-signal-popup 'append)
(zenirc-add-hook 'zenirc-dcc-chat-mode-hook 'turn-on-auto-fill)
(zenirc-add-hook 'zenirc-dcc-chat-mode-hook
                 (lambda (&rest ignored)
                   (setq zenirc-fill-mode t)
                   (make-local-variable 'zenirc-fill-message-categories)
                   (setq zenirc-fill-message-categories t)))

(zenirc-add-hook 'zenirc-exit-hook 'zenirc-signal-popup)


;; extreme silliness

(require 'zenirc-trigger)

(autoload 'horoscope           "horoscope" nil t)
(autoload 'shop-string         "shop"      nil t)
(autoload 'uboat-death-message "uboat"     nil t)
(autoload 'youwill             "youwill"   nil t)

(zenirc-trigger-register "8ball"     'zenirc-8ball        "\\b8.?ball\\b")
(zenirc-trigger-register "flame"     'flame-string        "\\bflame\\b")
(zenirc-trigger-register "horoscope" 'horoscope           "\\bstars\\b")
(zenirc-trigger-register "hype"      'youwill             "\\bhype\\b")
(zenirc-trigger-register "uboat"     'uboat-death-message "\\buboat\\b")

(zenirc-trigger-register "quest"
                         (lambda (&rest ignore)
                           (format "Your Quest: Seek the %s" (shop-string)))
                         "\\bquest\\b")

(setq zenirc-uboat-devices
      '(;("HOROSCOPES")
        ;("JOSHUA")
        ("SHOPPING LISTS")
        ("UBOAT MESSAGES")
        ;("VEEP-MODE")
        ("ZIPPY QUOTES")
        ))


(defun zenirc-disable-all-registered-triggers ()
  "If our behavior is strict, we do not need fun!"
  (interactive)
  (let ((table zenirc-trigger-table))
    (while table
      (zenirc-trigger-disable (car (car table)))
      (setq table (cdr table)))))

(defun zenirc-enable-all-registered-triggers ()
  (interactive)
  (let ((table zenirc-trigger-table))
    (while table
      (zenirc-trigger-enable (car (car table)))
      (setq table (cdr table)))))

(defun zenirc-be-a-stick-in-the-mud ()
  "If our behavior is strict, we do not need fun!"
  (interactive)
  (let ((re-list '("In this issue of Tiger Beat:"
                   "Our TigerInsider Feature:"
                   "Tiger Beat Presents:"
                   "and the company that will bring it to you: at&t"
                   "here is your shopping list: \\(([0-9]+) [^\n;]+;?\\)+"
                   "magic 8-ball says ====> [A-Z., \t]+$"
                   "YES I AM REALLY WRITTEN IN 3L33T FORTRAN 77!!!!!!!!!111"))
        (sym (if (boundp 'zenirc-ignore-list)
                 'zenirc-ignore-list
               'zenirc-ignorance-list)))
    (while re-list
      (or (member (car re-list) (symbol-value sym))
          (set sym (cons (car re-list) (symbol-value sym))))
      (setq re-list (cdr re-list)))))

(defun zenirc-be-boring ()
  (interactive)
  (zenirc-disable-all-registered-triggers)
  (zenirc-be-a-stick-in-the-mud)
  (zenirc-public-signal-mode -1))

;; For now.
(zenirc-add-hook 'zenirc-mode-hook 'zenirc-be-boring)


(add-hook 'same-window-regexps
          (concat "^" (regexp-quote zenirc-buffer-name)))

(setq zenirc-nick-default "noah")
(setq zenirc-port-default 6667)

;(add-list-members 'zenirc-notify-list
;  "ben"
;  "chelra"
;  "gnarla"
;  "hag"
;  "hodji"
;  "piglet3"
;  "womble")

(add-list-members 'zenirc-ignorance-list
  "^:NickServ!NickServ@services\\. \\(PRIVMSG\\|NOTICE\\) [^:\n]*:\\(This nickname is owned\\|If this is your nickname\\)")

(setq zenirc-timestamp-interval '(0 3600))
(setq zenirc-popup-ratio 6)
(setq zenirc-popup-min-height 5)

(setq-default zenirc-fill-mode nil)

;; The right kind of invalid ctcp can cause you to flood in reply with
;; error messages, kicking you off.  So don't do it at all.  Isn't the IRC
;; protocol so wonderfully robust?
(setq zenirc-send-ctcp-errmsg-on-unbalanced nil)
(setq zenirc-send-ctcp-errmsg-on-unknown nil)

(setq zenirc-dcc-chat-buffer-name-format "*%s*")

;; op anyone, anywhere, if they request it
(setq-default zenirc-iwantop-alist '((".*" ".*")))

;; create some permanently-enabled completions
(zenirc-complete-cache-list '("#/dev/gaf"
                              "#/mark/lar"
                              "#meditation"
                              "#mozilla"
                              )
                            'permanent)

;; zenirc-complete options
(setq zenirc-complete-add-final-space-p t)
(setq zenirc-complete-uncache-discarded-nicks t)
(set-alist-slot zenirc-complete-expire-time "." (* 60 60 24))

;; zenirc-msgstamp options
(setq zenirc-msgstamp-message-prefix-mode t)
(setq zenirc-msgstamp-message-inline-mode nil)
(setq zenirc-msgstamp-message-criteria 'always)
(setq zenirc-msgstamp-remove-default-name-mode t)
(setq zenirc-msgstamp-time-stamp-function
      (lambda () (format-time-string "%H:%M")))

(add-forms-to-after-load-alist
  '(("uboat"
     (apply 'add-list-members 'uboat-device zenirc-uboat-devices))))

(provide 'init-zenirc)

;;; init-zenirc.el ends here.
