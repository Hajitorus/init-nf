;;; init-fff.el --- fff customizations

;; Copyright (C) 1996, 1997, 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-fff.el,v 1.12 2007/12/30 02:56:21 friedman Exp $

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

(require 'fff)
(require 'fff-elisp)
(require 'fff-rfc)

(require 'dirtree)

;; This is not initialized to the full list on load because it can be
;; slow.  The relevant functions initialize this the first time.
(defvar ifff-misc-path nil)


;; ----------
(defun ifff-initialize-ifff-misc-path ()
  (setq ifff-misc-path
        (let* ((domain (dns-domain-name))

               (sinit (file-name-as-directory (getenv "sinit")))
               (sinit-ddirs (mapcar (lambda (s) (concat sinit s "/"))
                                    '("share" "")))
               (sinit-idirs '("CVS/" "RCS/" "emacs/" "bin/"))
               (sinit-pred (lambda (s)
                             (not (or (member default-directory sinit-ddirs)
                                      (member s sinit-idirs)))))

               (lpred (lambda (s)
                         ;; If the relative directory component ends with a
                         ;; `/', it is not a symlink but actually a
                         ;; dereference.
                         (and (char-equal (aref s (1- (length s))) ?/)
                              (setq s (substring s 0 -1)))
                         (not (file-symlink-p s))))

               (filter (lambda (s) (substring s 0 (1- (length s))))))
          (mapcar 'expand-file-name
                  (flatten-lists

                   (directory-tree sinit t sinit-pred filter)

                   "~/lib/autoconf"
                   "~/lib/bash"
                   "~/lib/calendars"
                   "~/lib/es"
                   "~/lib/misc"
                   "~/lib/perl"
                   "~/lib/ps"
                   "~/lib/tex/inputs"

                   "~/src/emacs/etc"
                   "~/src/toolbox"
                   "~/src/toolbox/cfuns"

                   "~/etc/misc"
                   "~/etc/humor"
                   "~/etc/html"
                   (concat "/home/www/users/" (user-login-name))
                   "~/etc/www"
                   (format "~/etc/org/%s" domain)
                   (format "~/etc/cron/tabs/%s" domain)
                   "~/etc/cron/diffmon"

                   "/etc"
                   "/usr/etc"
                   "/usr/local/etc"
                   "/usr/local/lib/std"

                   (directory-tree "/usr/local/gnu/include/" nil lpred filter)
                   (directory-tree "/usr/local/include/"     nil lpred filter)
                   "/usr/include"
                   (mapcar (lambda (s) (concat "/usr/include/" s))
                           '("sys"
                             "arpa" "net" "netinet"
                             "rpc" "rpcsvc"
                             "tcl"))
                   )))))

;; ----------
;;;###autoload
(defun ifff-find-file-in-ifff-misc-path (file &optional firstp)
  (interactive (list (fff-completing-read-file-in-path
                      "Find file (fff ifff-misc-path): "
                      (or ifff-misc-path 'ifff-initialize-ifff-misc-path))
                     current-prefix-arg))
  (or ifff-misc-path (ifff-initialize-ifff-misc-path))
  (fff-<op>-file-in-path file 'ifff-misc-path firstp fff-match-predicate
                         'find-file (interactive-p)))

;; ----------
;;;###autoload
(defun ifff-insert-file-in-ifff-misc-path (file &optional firstp)
  (interactive (list (fff-completing-read-file-in-path
                      "Insert file (fff ifff-misc-path): "
                      (or ifff-misc-path 'ifff-initialize-ifff-misc-path))
                     current-prefix-arg))
  (or ifff-misc-path (ifff-initialize-ifff-misc-path))
  (fff-<op>-file-in-path file 'ifff-misc-path firstp fff-match-predicate
                         'insert-file (interactive-p)))


;; The \(\) pair in front of the open paren insures that point will go at
;; the beginning of the sexp instead of the symbol name when calling
;; `fff-find-loaded-emacs-lisp-function-or-variable'.
(setq fff-emacs-lisp-def-regexp
      "^\\s-*\\(\\)(\\bdef\\w+\\s-+'?\\(%s\\)\\(\\s-\\|$\\)")

(fff-install-map t)
(fff-elisp-install-map)
(fff-rfc-install-map)

(fff-define-key "\C-m"
                'ifff-find-file-in-ifff-misc-path
                "Find file from misc path")

(fff-define-key "\C-i\C-m"
                'ifff-insert-file-in-ifff-misc-path
                "Insert file from misc path")

(provide 'init-fff)

;;; init-fff.el ends here.
