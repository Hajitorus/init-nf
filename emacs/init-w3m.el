;;; init-w3m.el --- initializations for w3m.el interface

;; Copyright (C) 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Created: 2004-04-12

;; $Id: init-w3m.el,v 1.7 2007/08/14 01:58:48 friedman Exp $

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

(require 'w3m)
(require 'browse-url)
(require 'host-fns)
(require 'fff)


;;;;;;
;;; My variables
;;;;;;

(defvar iw3m-buffer-name-format "w3m: %s")

(defvar iw3m-useless-buffer-names
  '("index.cgi" "index.htm" "index.html" "index.php" "index.shtml"))


;;;;;;
;;; Defuns
;;;;;;

;;;###autoload
(defun browse-url-w3m-emacs (url &optional new-session)
  "Ask the emacs-w3m browser to load URL in emacs.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new w3m session.  A non-nil interactive
prefix argument reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-SESSION is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "w3m URL: "))
  (w3m-browse-url url new-session))

;;;###autoload
(defun iw3m-name-buffer (url)
  (when (member (downcase (file-name-nondirectory url))
                iw3m-useless-buffer-names)
    (setq url (file-name-directory url)))
  (rename-buffer (format iw3m-buffer-name-format
                         (file-name-nondirectory (directory-file-name url)))
                 t))


;;;;;;
;;; W3M variables
;;;;;;

(setq w3m-fill-column                   78
      w3m-use-ange-ftp                  t
      w3m-profile-directory             "~/etc/misc/.w3m"
      w3m-default-display-inline-images t
      w3m-toggle-inline-images-permanently nil
      w3m-image-viewer                  (car (fff-files-in-directory-list
                                              '("xv" "eog" "gqview" "xloadimage" "display")
                                              exec-path t 'file-executable-p))
      w3m-use-form                      t
      w3m-use-cookies                   t
      w3m-track-mouse                   t
      w3m-use-tab                       t
      w3m-use-tab-menubar               t
      w3m-pop-up-windows                t
      w3m-pop-up-frames                 nil
      w3m-auto-show                     t
      w3m-use-filter                    nil ; use privoxy instead
      w3m-use-refresh                   nil)

;; Seek out the icon directory for emacs-w3m, which may not (in fact,
;; probably won't) be installed in the global data-directory.
(setq w3m-icon-directory
      (let ((d (car (fff-files-in-directory-list
                     '("icons30" "icons") load-path
                     'first 'file-directory-p))))
        (and d (file-name-as-directory d))))

;; Too many cases where w3m.el inspects the value of this variable before
;; it has been initialized from the default.  So change the default.
(setq-default w3m-display-inline-images w3m-default-display-inline-images)

;; Don't proxy hosts within the local dns domain
(let ((name (dns-domain-name))
      tld)
  (when (string-match "[^.]+\\.[^.]+$" name)
    (setq tld (match-string 0 name))
    (add-to-list 'w3m-command-arguments-alist
      (list (format "^http://\\([^/]*\\.\\)*%s\\(/\\|$\\)" tld)
            "-no-proxy")
      t)))


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(add-hook 'w3m-display-hook 'iw3m-name-buffer)

(add-hook 'w3m-mode-setup-functions (lambda () (set-buffer-multibyte t)))

;; This belongs in the w3m config file instead.
;(add-to-list 'w3m-command-arguments-alist
;  '("" "-o" "http_proxy=http://localhost:8118/"
;       "-o" "https_proxy=http://localhost:8118/")
;  t)

;; Make aliases for programs that think they're using w3 (e.g. vm)
;;
;; This is probably going to give me grief someday when some program
;; tries to make a call to w3's internals because it thinks it's loaded.
;; They ought to check (featurep 'w3), but don't count on it.
(defalias 'w3-region            'w3m-region)
(defalias 'w3-fetch             'w3m-browse-url)
(defalias 'w3-fetch-other-frame 'w3m-browse-url)
(defalias 'w3-find-file         'w3m-find-file)
(defalias 'w3-about             'ignore)

(provide 'init-w3m)

;;; init-w3m.el ends here
