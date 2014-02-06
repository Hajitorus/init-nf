;;; init-mail.el --- mail composition and related mode initializations

;; Copyright (C) 1991, 99, 2009 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-mail.el,v 1.14 2010/01/26 03:14:52 friedman Exp $

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

;; I use `imail' ("init mail") to prefix some of my mail extensions.

;;; Code:

(require      'init-common)
(require-soft 'order-head)
(require-soft 'fmailutils)
(require-soft 'bbdb)

(provide 'init-mail)


;;;;;;
;;; Personal variable declarations
;;; These are not variables defined in other packages, but rather they are
;;; declarations for variables I created and use in my initialization files.
;;;;;;

;; ----------
;; Standard in Emacs 19.29 or later, but not declared in prior versions.
(defvar mail-send-hook nil
  "Normal hook run before sending mail, in Mail mode.")


;;;;;;
;;; Macros
;;;;;;

;; Define some mail header rewriters for different addresses I use.
(defmacro define-mail-from-addr (sym addr)
  (setq sym (intern (concat "mail-from-" (symbol-name sym))))
  (list 'defun sym '()
        '(interactive)
        (list 'mail-from-addr addr)))


;;;;;;
;;; Defuns (some are modified or extended versions of standard functions)
;;;;;;

;; ----------
(defun imail-abbrev-complete-abbrev (&optional start-pos)
  (interactive)
  (cond ((mail-abbrev-in-expansion-header-p)
         (let* ((completion-ignore-case t)
                (w (current-word 'strict))
                (r (current-word-region 'strict))
                (c (try-completion w mail-abbrevs)))
             (cond
              ((eq c t)
               (message "Unique completion"))
              ((eq c nil)
               (cond ((fboundp 'ibbdb-complete-name)
                      (if (interactive-p)
                          (call-interactively 'ibbdb-complete-name)
                        (ibbdb-complete-name start-pos)))
                     (t
                      (message "No match for \"%s\" in mail aliases" w))))
              ((string= (downcase w) (downcase c))
               (with-output-to-temp-buffer " *Completions*"
                   (display-completion-list (all-completions w mail-abbrevs))))
              (t
               (goto-char (car r))
               (delete-region (car r) (cdr r))
               (insert c)))))
        (t
         ;; arbitrary choice, but usually what I want.
         (call-interactively 'lisp-complete-symbol))))

;; ----------
;; This avoids expanding a bug that caused mail abbrevs to be expanded
;; twice in emacs 19.34 and earlier.
(defadvice mail-abbrev-expand-hook (around sinit:indent-relative activate)
  "Disable abbrev-mode around call to indent-relative."
  (let ((abbrev-mode nil))
    ad-do-it))

;; ----------
(defun imail-add-fcc-related-headers-always ()
  (fmailutils-add-fcc-related-headers t))

;; ----------
(defun imail-from-addr (&optional addr)
  (interactive "sHost or domain name:")
  (fmailutils-set-from-address addr nil '<> t))

(defalias 'mail-from-addr 'imail-from-addr)

;; ----------
(defun imail-mode-initialize ()
  ;; When mail-aliases is t, sendmail tries to rebuild it.
  ;; That isn't necessary since mail-abbrevs may be used instead.
  ;; Unfortunately, doing this in mail-setup-hook is too late;
  ;; mail-setup already builds mail-aliases before
  ;; mail-setup-hook is called, and it is on the latter that
  ;; build-mail-abbrevs (which is autoloaded) gets called.
  (and (boundp 'mail-setup-hook)
       (memq 'mail-abbrevs-setup mail-setup-hook)
       (setq mail-aliases nil))

  (save-match-data
    (when (string-match "%" (default-value 'mail-archive-file-name))
      (make-local-variable 'mail-archive-file-name)
      (setq mail-archive-file-name
            (format-time-string (default-value 'mail-archive-file-name)
                                (current-time)))))

  ;; Make filladapt 2.11+ indent header continuation lines.
  (cond ((and (boundp 'filladapt-token-table)
              (boundp 'filladapt-version)
              (string-lessp "2.10" filladapt-version))
         (make-local-copied-variables 'filladapt-token-table)
         (set-alist-slot filladapt-token-table
                         "^[^:\n\t ]+: " '(bullet) nil nil t)))
  (override-default-variable-settings))


;; ----------
(defadvice mail-send (before sinit:mail-send-hook activate)
  (run-hooks 'mail-send-hook))

;; ----------
(defun imail-setup ()
  (setq default-directory mail-folder-directory)

  (delete-auto-save-file-if-necessary)
  (setq buffer-auto-save-file-name
        (concat mail-folder-directory
                (file-name-nondirectory
                 (make-auto-save-file-name))))

  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions
            'make-autosave-for-buffer-before-kill-p))


;;;;;;
;;; Mode hooks
;;;;;;

(add-hook 'mail-mode-hook 'imail-mode-initialize)

(add-hook 'mail-setup-hook 'mail-abbrevs-setup)
(add-hook 'mail-setup-hook 'imail-setup)
(add-hook 'mail-setup-hook 'imail-from-addr)


;;;;;;
;;; Variables
;;;;;;

(setq mail-folder-directory          (expand-file-name "~/etc/mail/personal/")
      mail-use-rfc822                t
      mail-yank-prefix               ">")

;; This variable was first made available in Emacs 19.26.
;; I use it personally regardless of the version.
(setq user-mail-address (concat (user-login-name) "@" (dns-domain-name)))

;; The default was changed from nil to t in emacs 19.29 to make mail-setup
;; initialize it from the envvar REPLYTO.  But I have my own mechanism for
;; doing this, and it also breaks vm-mail-internal in vm 5.72, which
;; doesn't expect to be anything but nil or a string.  Turn it off.
(setq mail-default-reply-to nil)

(setq mail-signature      nil
      mail-signature-file "/dev/null")

;; mailabbrev uses mail-personal-alias-file in 19.29 and later, but in
;; prior versions it had its own variable.
(and (boundp 'mail-personal-alias-file)
     (setq mail-abbrev-mailrc-file mail-personal-alias-file))


;;;;;;
;;; External libraries to load
;;;;;;

;; Override any previous autoloads and re-autoload these.
(cond ((memq (emacs-variant) '(xemacs lucid))
       ;; Emacs compatibility
       (defalias 'build-mail-abbrevs 'build-mail-aliases)
       (defalias 'mail-abbrevs-setup 'mail-aliases-setup)
       ;; Force redefinition by mail-abbrev library.
       (fmakunbound 'build-mail-aliases)
       (fmakunbound 'mail-aliases-setup)
       (override-autoloads
         '((build-mail-aliases . "mail-abbrev")
           (mail-aliases-setup . "mail-abbrev"))))
      (t
       (override-autoloads
         '((build-mail-abbrevs . "mailabbrev")
           (mail-abbrevs-setup . "mailabbrev")))))

(add-forms-to-after-load-alist
  '(("bbdb"
     (bbdb-initialize 'sendmail))

    ("fmailutils"
     (add-hook 'mail-send-hook 'imail-add-fcc-related-headers-always))

    ("mail-hist"
     ;; In emacs 19.30 and later, mail-hist does not install itself on the
     ;; appropriate hooks automatically upon loading.
     (and (fboundp 'mail-hist-enable)
          (mail-hist-enable))
     ;; VM 5.72 runs vm-mail-mode-hook after mail-mode-hook, the latter of
     ;; which changes the keymap that was in use when mail-hist-define-keys
     ;; was run.  So put mail-hist-define-keys on vm-mail-mode-hook too.
     (and (fboundp 'mail-hist-define-keys)
          (add-hook 'vm-mail-mode-hook 'mail-hist-define-keys)))

    ("mail-abbrev"
     (eval-after-load-forms-for "mailabbrev"))

    ("mailabbrev"
     (let ((map '(("\C-n"    . mail-abbrev-next-line)
                  ("\C-i"    . tab-to-tab-stop)
                  ("\M->"    . mail-abbrev-end-of-buffer)
                  ("\M-\C-i" . imail-abbrev-complete-abbrev))))

       (keymap-define-keys mail-mode-map map)

       (add-after-load-alist "vm-vars"
         `(keymap-define-keys vm-mail-mode-map (quote ,map))))

     (setq mail-abbrev-mode-regexp
           "^\\(Resent-\\)?\\(To\\|From\\|Reply-To\\|CC\\|BCC\\):")

     (load "sendmail-alias" t))

    ("mailalias"
     (load "sendmail-alias" t))

    ("mailcrypt"
     (add-hook 'vm-mail-mode-hook    'mc-install-write-mode)
     (add-hook 'vm-mode-hook         'mc-install-read-mode)
     (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
     (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode))

    ("order-head"
     (add-hook 'fmailutils-set-from-address-hook
               'mail-reorder-headers))

    ("sendmail-alias"
     (setq mail-personal-alias-file
           (format "%s/share/aliases" (getenv "sinit"))))

    ("silly-mail"
     (add-hook 'mail-setup-hook 'sm-add-random-header))

    ("vm"
     (load-offer-compile "init-vm" t))

    ("nuke-whitespace"
     (add-hook 'mail-send-hook 'nuke-trailing-whitespace))
    ))

(add-after-load-libraries
  "mail-hist")

(load-offer-compile "all.mail" t)

;;; init-mail.el ends here.
