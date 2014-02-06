;;; init-path.el --- set the load-path for emacs

;; Copyright (C) 1994, 95, 96, 99, 00, 03, 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-path.el,v 1.19 2009/01/18 19:25:50 friedman Exp $

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

(require 'init)
(require 'dirtree)

(defun init-standard-load-path (&rest pathlists)
  (interactive)
  (let* ((build-subdirectory-list-file-name ".init-subdirs.els")
         (prefix (expand-file-name "~/lib/elisp"))
         (prefix-version (format "%s/%d" prefix (emacs-version-major)))
         (prefix-variant-version (format "%s/%s%d"
                                         prefix
                                         (emacs-version-prefix)
                                         (emacs-version-major)))
         (prefix-variant-versionc (concat prefix-variant-version "c"))

         (sysname (system-name))
         (sysname-list nil)
         (pos 0)
         (tem nil))
    (save-match-data
      (while (string-match "\\." sysname pos)
        (setq sysname-list (cons (substring sysname pos (match-beginning 0))
                                 sysname-list))
        (setq pos (match-end 0)))
      (setq sysname-list
            (reverse (mapcar (lambda (s)
                               (setq tem (if tem (concat tem "." s) s))
                               (concat prefix "/domain/" tem))
                             ;; get component after last `.', missed in loop
                             (cons (substring sysname pos) sysname-list)))))

    (set-load-path (concat prefix-variant-versionc "/.init")
                   ;;(concat (getenv "sinit") "/local/emacs")
                   ;;(concat (getenv "sinit") "/emacs")
                   (mapcar 'build-subdirectory-list-deep
                           (list (concat prefix "/init")
                                 prefix-variant-versionc
                                 prefix-variant-version
                                 prefix-version
                                 (concat prefix "/common")
                                 (concat prefix "/noahf")))
                   (mapcar 'build-subdirectory-list-deep sysname-list)
                   (concat prefix "/domain/site-init")
                   (delete prefix (build-subdirectory-list-deep prefix))
                   pathlists
                   load-path
                   ;; append some fedora package directories
                   (mapcar (lambda (d)
                             (directory-file-name
                              (concat "/usr/share/emacs/site-lisp/" d)))
                           '("apel"
                             "bbdb"
                             "emacspeak/lisp"
                             "flim"
                             "maxima"
                             "nxml-mode"
                             "psgml"
                             "ruby-mode"
                             "vm"
                             "wl"
                             "w3m"
                             "")))))


(init-standard-load-path)

(provide 'init-path)

;;; init-path.el ends here.
