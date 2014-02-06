;;; init-autoloads.el

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1992-03-31
;; Public domain

;; $Id: init-autoloads.el,v 1.46 2011/12/05 21:41:20 friedman Exp $

;;; Commentary:
;;; Code:


(require 'list-fns)

;; v18 does not have this library.
(or (featurep 'autoload)
    (load "autoload" t))

(defvar init-autoloads-directories
  (list "~/lib/elisp/init/"
        "~/lib/elisp/noahf/"
        (concat (getenv "sinit") "/emacs/")))

(defvar init-autoloads-file ".autoloads.el")

(defadvice make-autoload (after sinit:autoload-delete-doc)
  (cond ((memq (car ad-return-value) '(autoload))
         (setcar (nthcdr 3 ad-return-value) nil))
        ((memq (car (ad-get-arg 0)) '(defvar defcustom))
         (setq ad-return-value (copy-sequence (ad-get-arg 0)))
         (setcar (nthcdr 3 ad-return-value) nil))))

(defun init-autoloads-make-autoloads (directory)
  (interactive "DUpdate autoloads for directory: ")
  (let* ((default-directory (file-name-as-directory directory))
         (generated-autoload-file (expand-file-name init-autoloads-file))
         (make-backup-files nil))
    ;; update-autoloads chokes if the autoloads file is empty or nonexistent.
    (cond ((and (file-exists-p init-autoloads-file)
                (> (nth 7 (file-attributes init-autoloads-file)) 0)))
          (t
           (save-excursion
             (set-buffer (find-file-noselect init-autoloads-file))
             (insert "\f\n")
             (save-buffer))))
    (unwind-protect
        (progn
          (ad-enable-advice 'make-autoload 'after 'sinit:autoload-delete-doc)
          (ad-activate 'make-autoload)
          (cond ((fboundp 'update-autoloads-from-directory)
                 (update-autoloads-from-directory directory))
                ((fboundp 'update-autoloads-from-directories)
                 (update-autoloads-from-directories directory))
                ((fboundp 'update-directory-autoloads)
                 (update-directory-autoloads directory))
                (t (error "No autoload updating function to call"))))
      (ad-disable-advice 'make-autoload 'after 'sinit:autoload-delete-doc))))

(defun init-autoloads-make-autoloads-all-directories ()
  (interactive)
  (mapc (lambda (dir)
          (when (file-directory-p dir)
            (init-autoloads-make-autoloads dir)))
    init-autoloads-directories))

(defun init-autoloads-load-all-directories ()
  (interactive)
  (mapc (lambda (l)
          (load (concat (file-name-as-directory l)
                        init-autoloads-file) t))
    init-autoloads-directories))


;;; Static autoloads below.

(autoload 'bbdb         "bbdb-com"     nil t)
(autoload 'bbdb-company "bbdb-com"     nil t)
(autoload 'bbdb-completing-read-record "bbdb")
(autoload 'bbdb-create-ftp-site        "bbdb-ftp")
(autoload 'bbdb-display-records        "bbdb")
(autoload 'bbdb-ftp                    "bbdb-ftp")
(autoload 'bbdb-insinuate-gnus         "bbdb-gnus")
(autoload 'bbdb-insinuate-mh           "bbdb-mh")
(autoload 'bbdb-insinuate-rmail        "bbdb-rmail")
(autoload 'bbdb-insinuate-sendmail     "bbdb")
(autoload 'bbdb-insinuate-vm           "bbdb-vm")
(autoload 'bbdb-name    "bbdb-com"     nil t)
(autoload 'bbdb-net     "bbdb-com"     nil t)
(autoload 'bbdb-notes   "bbdb-com"     nil t)

;; acl2 mode autoloads
(autoload 'run-acl2  "load-inferior-acl2" nil t)
(autoload 'acl2-mode "load-inferior-acl2" nil t)

;; Babyl-to-vm conversion autoloads
(autoload 'rmail-hierarchy-to-vm "babyl-vm" nil t)
(autoload 'rmail-folder-to-vm    "babyl-vm" nil t)
(autoload 'rmail-buffer-to-vm    "babyl-vm" nil t)
(autoload 'rmail-message-to-vm	 "babyl-vm" nil t)

;; bibl-mode from Bryan O'Sullivan <bosullvn@maths.tcd.ie>
(autoload 'bibl-visit-bibliography "bibl-mode" nil t)
(autoload 'bibl-mode               "bibl-mode" nil t)

;;(autoload 'gnuserv-start     "gnuserv-compat"  nil t)
;;(autoload 'gnuserv-running-p "gnuserv-compat"  nil t)

(autoload 'gopher                 "gopher" nil t)
(autoload 'gopher-dispatch-object "gopher" nil t)

;; Ispell autoloads
(autoload 'ispell-word          "ispell" nil t)
(autoload 'ispell-complete-word "ispell" nil t)
(autoload 'ispell-region        "ispell" nil t)
(autoload 'ispell-buffer        "ispell" nil t)
(autoload 'ispell               "ispell" nil t)

;; Lisp code directory archive stuff.
(autoload 'format-lisp-code-directory "lispdir" nil t)
(autoload 'lisp-dir-apropos           "lispdir" nil t)

;; My LPF frobs
(autoload 'lpf-reply   "lpf-frobs" nil t)
(autoload 'lpf-reply-1 "lpf-frobs" nil t)

;; Mview (mike william's view-mode) commands
(autoload 'mview-file   "mview" nil t)
(autoload 'mview-buffer "mview" nil t)
(autoload 'mview-mode   "mview" nil t)

;; python-mode autoloads
;(autoload 'python-mode "python-mode" nil t)
;(autoload 'py-shell    "python-mode" nil t)

;; VM autoloads
(autoload 'vm              "vm" nil t)
(autoload 'vm-visit-folder "vm" nil t)
(autoload 'vm-peek         "vm" nil t)

;; Vkill commands
(autoload 'vkill                  "vkill" nil t)
(autoload 'list-unix-processes    "vkill" nil t)
(autoload 'process-list-vkill     "vkill" nil t)
(autoload 'process-list-all-vkill "vkill" nil t)

;; Miscellaneous autoloads
(autoload 'dos-mode            "dos"            nil t)
(autoload 'edebug-defun        "edebug"         nil t)
(autoload 'forms-mode          "forms"          nil t)
(autoload 'horoscope           "horoscope"      nil t)
(autoload 'invoice             "invoice"        nil t)
(autoload 'jam-mode            "jam-mode"       nil t)
(autoload 'jump-to-def         "jump-def"       nil t)
(autoload 'live-find-file      "live-find-file" nil t)
(autoload 'Man-cleanup-manpage "man"            nil t)
(autoload 'pp-eval-last-sexp   "pp"             nil t)
(autoload 'remap-del-key       "remap-del-key"  nil t)
(autoload 'uboat-death-message "uboat"          nil t)
(autoload 'update-dns-serial   "update-dns"     nil t)
(autoload 'youwill             "youwill"        nil t)
(autoload 'zenicb              "zenicb"         nil t)
(autoload 'zenirc              "zenirc"         nil t)

;; Load generated autoloads, but only after all variables have been
;; initialized in other init-* files.  Otherwise, library defcustoms may be
;; initialized prematurely.
(and (eq (emacs-variant) 'emacs)
     (add-hook 'after-do-inits-hook 'init-autoloads-load-all-directories))

(provide 'init-autoloads)

;; init-autoloads.el ends here
