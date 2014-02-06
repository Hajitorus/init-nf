;;; Gnus frobs --- need gnus 4.1 (from emacs 19) or later to work!

;; Copyright (C) 1993, 1994, 1995, 1996 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-gnus.el,v 1.28 2004/02/16 10:56:36 friedman Exp $

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
;;; Code:

(require 'gnus)
(require 'host-fns)
(require-soft 'rmailout)
(require-soft 'mail-utils)

(or (fboundp 'rmail-file-p)
    (defalias 'rmail-file-p 'mail-file-babyl-p))

(defvar gnus-rfc934-forwarding t
  "*If non-nil, use RFC934-style forwarding.")

(defun gnus-forward-article ()
  "Compose a mail message with current article as initial contents.
If gnus-rfc934-forwarding is non-nil, then modify body of forwarded
message to conform to RFC934."
  (interactive)
  (let* ((article-buffer-name "*Article*")
         (transient-mark-mode nil)
         (zmacs-regions nil)
         mail-subject-line)
    (save-excursion
      (set-buffer article-buffer-name)
      (setq mail-subject-line
            (concat "[" (or (mail-strip-quoted-names
                             (gnus-fetch-field "From")) "")
                    ": " (or (gnus-fetch-field "Subject") "") "]")))
    (gnus-summary-toggle-header 1)
    (other-window 1)
    (mail nil nil mail-subject-line)
    (save-excursion
      (goto-char (point-max))
      (insert (format "------- start of forwarded message %s-------\n"
                      (if gnus-rfc934-forwarding
                          "(RFC 934 encapsulation) "
                        "")))
      (insert-buffer article-buffer-name)
      (save-excursion
        (save-window-excursion
          (gnus-summary-toggle-header 0)))
      (and gnus-rfc934-forwarding
           (save-restriction
             (let ((begin (point))
                   (end (mark)))
               (widen)
               (narrow-to-region begin end)
               (goto-char (point-min))
               (while (and (< (point) end) (re-search-forward "^-" end t))
                 (replace-match "- -" t t)
                 (forward-char)))))
      (goto-char (point-max))
      (insert "------- end of forwarded message -------\n"))))

(defun gnus-my-disable-version-numbers (&rest ignored)
  (make-local-variable 'version-control)
  (setq version-control nil))


(setq
   gnus-home-directory                  (expand-file-name "~/etc/gnus/")
   gnus-directory                       gnus-home-directory

   gnus-article-save-directory		gnus-directory
   gnus-auto-extend-newsgroup 		t
   gnus-auto-select-first               nil
   gnus-auto-select-same                t
   gnus-break-pages 			nil   ; t will break forwarding
   gnus-default-article-saver           'gnus-summary-save-in-mail
   gnus-default-distribution            "world"
   gnus-default-distributions           '("world")    ; emacs 19.29 or later
   gnus-interactive-exit 		nil
   gnus-interactive-post 		nil
   gnus-kill-files-directory		gnus-article-save-directory
   gnus-large-newsgroup                 50
   gnus-mail-forward-method 		'gnus-mail-forward-using-mail
   gnus-nntp-server                     (or (and (boundp 'gnus-nntp-server)
                                                 gnus-nntp-server)
                                            (getenv "NNTPSERVER")
                                            "nntp")
   gnus-novice-user                     nil
   gnus-organization-file               (format "%s%s" gnus-directory ".organization")
   gnus-save-all-headers                t
   gnus-show-threads                    t
   gnus-startup-file                    (format "%s%s" gnus-directory ".gnusrc")
   gnus-thread-hide-killed              t
   gnus-thread-hide-subject             nil
   gnus-thread-hide-subtree             nil
   gnus-thread-ignore-subject           t
   gnus-thread-indent-level 		2
   gnus-use-cross-reference             'always
   gnus-use-followup-to                 t
   gnus-use-full-window                 nil
   gnus-use-generic-from                t
   gnus-use-generic-path                nil
   gnus-use-long-file-name              t
   gnus-your-domain                     (or (and (boundp 'gnus-your-domain)
                                                 gnus-your-domain)
                                            (dns-domain-name))
   nntp-large-newsgroup                 20
   nntp-authinfo-file                   (format "%s%s" gnus-directory ".authinfo")
)

;; sgnus variables
(setq gnus-check-new-newsgroups 	t       ; 'ask-server
      gnus-expert-user 			nil	; t makes gnus too mute
      gnus-group-default-list-level     3
      gnus-group-goto-unread 		t
      gnus-inhibit-startup-message 	t
      gnus-no-groups-message            "No news is good news"
      gnus-prompt-before-saving 	t
      gnus-read-active-file 		'some ; t
      gnus-save-newsrc-file 		nil
      gnus-save-newsrc-hook 	        'gnus-my-disable-version-numbers
      gnus-save-quick-newsrc-hook 	'gnus-my-disable-version-numbers
      gnus-save-standard-newsrc-hook 	'gnus-my-disable-version-numbers
      gnus-select-method 		(list 'nntp gnus-nntp-server)
      gnus-summary-goto-unread 		t
      gnus-summary-make-false-root	'adopt
      gnus-thread-sort-functions        '(gnus-thread-sort-by-subject)
      gnus-verbose 			7
      gnus-group-buffer                 "*Newsgroups*"
      gnus-mode-non-string-length       nil
      )

(setq gnus-secondary-servers
      '(
        "news.speakeasy.net"
        "news.supernews.com"
        "news.mozilla.org"
        "nntp.hks.net"
        ;;"life.ai.mit.edu"
        ;;"nntp.lcs.mit.edu"
        ))

(define-key gnus-summary-mode-map "z" 'gnus-forward-article)

(add-forms-to-after-load-alist
  '(("bbdb"
     (bbdb-initialize 'gnus 'message))))


(provide 'init-gnus)

;;; init-gnus.el ends here.
