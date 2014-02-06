;;; init-w3.el --- initializations for Bill Perry's elisp web browser

;; Copyright (C) 1994-97 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Created: 1994-04-16

;; $Id: init-w3.el,v 1.16 2000/11/22 06:58:04 friedman Exp $

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

;; This file contains expressions and variables that are loaded and
;; evaluated upon startup of W3 (the www browser for emacs).

;;; Code:

(and (string-lessp w3-version-number "3.0.86")
     (error "W3 version %s is too old to work with w3-init.el"
            w3-version-number))

;; Len Tower is particularly fond of using nonexistent link types.
;; Define them and make them no-ops.
(url-register-protocol "phoneto" 'identity)
(url-register-protocol "faxto" 'identity)

(setq my-w3-directory (expand-file-name "~/etc/www/w3/"))

(setq w3-annotation-position           'bottom
      ;; New in W3 3.0.  As far as I can tell, much of the old w3 faces
      ;; code and perhaps other things pertaining to presentation aren't
      ;; coded directly in lisp anymore, but use stylesheets instead.
      ;; See the file (my-w3-directory)/stylesheet.
      w3-configuration-directory       my-w3-directory
      w3-confirmation-func             'y-or-n-p
      ;;w3-default-homepage            nil      ; find a good one for this.
      w3-default-stylesheet            (concat my-w3-directory "stylesheet")
      ;; Let's see how annoying this really is.
      ;; Probably it's only implemented for xemacs.
      w3-do-blinking                   nil
      w3-do-incremental-display        nil   ; w3-3.0
      w3-hotlist-file                  (concat my-w3-directory "hotlist")
      w3-keep-old-buffers              t

      w3-mail-command                  'mail
      w3-mail-other-window-command     'mail-other-window
      w3-personal-annotation-directory (concat my-w3-directory "annotations")
      w3-reuse-buffers                 nil          ; 'yes or 'no
      w3-right-border                  0
      w3-show-headers                  nil
      w3-show-status                   t
      w3-strict-width                  75
      w3-delay-image-loads             t
      ;; might consider setting this to t if too many pages set weird
      ;; colors emacs treats the same as background on mono displays,
      ;; making the text unreadable.
      ;;
      ;; Bill Perry suggests the following, using XEmacs 19.12 calls that
      ;; are implemented by w3-sysdp.el for other emacsen, but I'm not
      ;; ready to rely on it just yet.
      ;;
      ;;   (setq w3-user-colors-take-precedence
      ;;         (and (not (eq (device-type) 'tty))
      ;;              (not (eq (device-class) 'mono))))
      ;;
      w3-user-colors-take-precedence   (cond (t t);default yes
                                             ((eq window-system 'x)
                                              (not (x-display-color-p)))
                                             (t t))
      w3-user-fonts-take-precedence    t
)

(setq url-bad-port-list                '("25" "119")
      url-confirmation-func            'y-or-n-p
      url-connection-retries           0
      url-inhibit-mime-parsing         nil
      url-keep-history                 t
      url-global-history-file          (concat my-w3-directory "history")
      ;; implemented in version 2.1.114
      url-honor-refresh-requests       'ask
      url-pgp/pem-entity               user-mail-address
      url-personal-mail-address        url-pgp/pem-entity
      ;; information not to send; implemented in version 2.1.114
      ;; another symbol you can add is 'os, but I don't care if people know
      ;; what OS I'm using.  My concern about collecting lastloc statistics
      ;; is that it may violate the privacy of other people's web pages.
      ;; And lastly, I don't like to give out my email address because less
      ;; scrupulous organizations will add you to their mailing lists
      ;; (which consist of advertisements) just for visiting their pages.
      url-privacy-level                '(email lastloc)
      url-show-status                  t
      url-use-hypertext-dired          nil
      url-use-hypertext-gopher         t
)

;; This seems to be buffer-local, at least in w3 2.1.118.
(setq-default url-be-asynchronous t)

;; I am getting sick of people constantly fucking around with modeline.
;; VM, and probably GNUS, do it too.
(setq w3-modeline-format
      '( ""
         mode-line-modified
         mode-line-buffer-ident-prefix
         mode-line-buffer-identification
         " " global-mode-string " "
         (line-number-mode ("L%l C%c") ((-3 . "%p")))
         " %[("
         mode-name
         mode-line-process
         "%n"
         (w3-current-isindex "[Searchable]  ")
         minor-mode-alist
         ")%]"
         (buffer-directory-file-name
          (" "
           buffer-directory-file-name
           (global-mode-string-trailer ("") (" "))))
         global-mode-string-trailer
         "%-"))

(cond ((eq window-system 'x)
       (and (not (x-display-color-p))
            ;; avoid defining color faces on mono displays as a result of html
            ;; such as <BODY background="" bgcolor="#ffffff" ...>
            (setq w3-user-colors-take-preference t)
            ;; name changed in 2.1.118 or something.  My idea, I'm afraid.
            (setq w3-user-colors-take-precedence t))))


;; Recently, some people have been automagically subscribed to
;; mailing lists by web servers at .com sites, just for connecting.
;; It usually consists of things like new product announcments.
;; Foil this bogosity.
;(setq url-personal-mail-address "postmaster@localhost")


(add-forms-to-after-load-alist
  '(("bbdb"
     (bbdb-initialize 'w3))))

(provide 'init-w3)

;;; init-w3.el ends here
