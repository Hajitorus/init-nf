;;; init-info.el --- initialization for info mode

;; Copyright (C) 1991, 92, 93, 94, 96, 99, 2001 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1991-07-11

;; $Id: init-info.el,v 1.19 2006/11/29 05:03:25 friedman Exp $

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

(require 'face-fns)

;;;;;;
;;; Variables
;;;;;;

;; Initialize for emacs 18, if necessary.
(defvar Info-default-directory-list)

;;;;;;
;;; Defuns
;;;;;;

(defun iinfo-setup-faces ()
  (let ((xref 'info-xref)
        (node 'info-node)
        (color "light goldenrod"))
    ;; Emacs 19 got these backward!
    (and (= (emacs-version-major) 19)
         (setq xref
               (prog1 node
                 (setq node xref))))

    ;; quote keywords for the sake of emacs 19
    (override-face-attributes xref
      ':foreground color ':bold nil ':underline nil)
    (override-face-attributes node
      ':foreground color ':bold nil ':underline nil))

  (mapc (lambda (face)
          (and (facep face)
               (override-face-attributes face ':foreground "cyan")))
    '(Info-title-1-face Info-title-2-face Info-title-3-face)))

;;;;;;
;;; Variables
;;;;;;

;; prepend
(add-list-members 'Info-default-directory-list
  (expand-file-name "~/lib/info/")
  (expand-file-name "~/lib/info/perl/current/"))

;; append
(apply 'append-list-members 'Info-default-directory-list
  (delq nil (list (and (boundp 'installation-directory)
                       installation-directory
                       (concat installation-directory "info/"))
                  (and (boundp 'configure-info-directory)
                       configure-info-directory
                       (file-name-as-directory configure-info-directory))
                  "/usr/share/info/"
                  "/usr/info/"
                  "/com/doc/info/"
                  )))

;; filter out non-existent
(mapc (lambda (d)
        (unless (file-directory-p d)
          (setq Info-default-directory-list
                (delq d Info-default-directory-list))))
  Info-default-directory-list)



(defvar Info-default-annotations-path nil)
(add-list-members 'Info-default-annotations-path
  (expand-file-name "~/lib/info/.info_notes")
  (expand-file-name "~/.info_notes")
  "/usr/local/info/INFO_NOTES")

(mapc (lambda (pair)
        (set-alist-slot 'Info-suffix-list (car pair) (cdr pair)))
  '((".info"     . nil)
    (".bz2"      . "bunzip2")
    (".info.bz2" . "bunzip2")))

(setq Info-enable-edit               nil
      Info-enable-active-nodes       t        ; may consider disabling this
      Info-fontify                   t
      Info-fontify-maximum-menu-size moby-bignum
      Info-directory-list            (or (getenv "INFOPATH")
                                         Info-default-directory-list))

(cond ((eq (emacs-variant) 'emacs)
       (iinfo-setup-faces)
       ;; Emacs 19 resets the faces each time; override this.
       (add-hook 'Info-mode-hook 'iinfo-setup-faces)))


;; XEmacs-only settings.
;; These are Dave Gillespie extensions.

(setq Info-novice              nil
      Info-restoring-point     t
      Info-auto-advance        'twice
      Info-footnote-tag        "See")

(setq Info-annotations-path    (or (getenv "INFONOTES")
                                   Info-default-annotations-path))

(provide 'init-info)

;;; init-info.el ends here.
