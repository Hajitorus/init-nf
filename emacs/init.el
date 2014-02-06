;;; init.el --- bootstrapping for my emacs init files

;; Copyright (C) 1994, 95, 96, 97, 98, 99, 02, 03, 04, 07, 2008 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init.el,v 1.38 2010/03/23 07:27:53 friedman Exp $

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

;; Unlike most other parts of my init files, this file cannot be
;; byte-compiled and loaded since it is this very code which determines
;; where to find the bytecode for particular emacs variants, make sure they
;; are up to date, etc.  On the other hand, I still want them to be fast.
;;
;; Thus, some functions here are declared via `defun-compile', which
;; compiles the function after it is defined unless the variable
;; `defun-compile-compile-p' is set to nil.  See bytecomp-fns.el.

;;; Code:

;; ----------
(defun lisp-indent-like (function like)
  "Indent FUNCTION in the same mannner as LIKE, another function.
This copies LIKE's `lisp-indent-function' property to FUNCTION."
  (put function 'lisp-indent-function (get like 'lisp-indent-function)))

;; ----------
;; Don't use backquote here, since syntax changed in later versions
(lisp-indent-like 'init-eval-and-compile-when 'when)
(defmacro init-eval-and-compile-when (pred &rest body)
  (list 'eval-and-compile (list 'cond (cons pred body))))

;; ----------
(lisp-indent-like 'init-eval-and-compile-unless 'unless)
(defmacro init-eval-and-compile-unless (pred &rest body)
  ;;`(eval-and-compile (cond ((not ,pred) ,@body)))
  (list 'eval-and-compile (list 'cond (cons (list 'not pred) body))))

;; ----------
(lisp-indent-like 'defun-undefined 'defun)
(defmacro defun-undefined (fn param &rest body)
  ;; `(or (fboundp ',fn) (defun ,fn ,param ,@body))
  (list 'or (list 'fboundp (list 'quote fn))
        (nconc (list 'defun fn param) body)))

(lisp-indent-like 'defalias-undefined 'defalias)
(defun defalias-undefined (fn defn &rest rest)
  (or (fboundp fn)
      (apply 'defalias fn defn rest)))


;; XEmacs 19.13 does not provide 'bytecomp, only 'byte-compile.
(require 'byte-compile "bytecomp")

;; Can't use defalias-undefined on this!
(or (fboundp 'defalias)
    (fset 'defalias 'fset))

(let ((load-path (cons (expand-file-name "~/lib/elisp/noahf") load-path)))
  (require 'emacs-variants)
  (require 'load-fns)
  (require 'bytecomp-fns))

;; ----------
(defvar after-do-inits-hook nil
  "*Forms to run after all else in `do-inits'.
This hook is a place for my init code to defer operations which depend
on later files being loaded.  For example, many libraries can't be
loaded until after all load-path manipulating forms have been run.")

;; ----------
(defvar after-load-libraries nil
  "*Libraries to load after all other forms in do-inits.
These libraries are loaded even after `after-do-inits-hook' is run.")

;; ----------
(defvar before-do-inits-time nil
  "Value of `current-time' before `do-inits' begins initialization.")

;; ----------
;; Note that after-init-time in v23 does not (as of 2008-07-21 cvs
;; snapshots) include the time taken to run after-init-hook.
(defvar after-do-inits-time nil
  "Value of `current-time' after `do-inits' ends.
This value is set after `after-do-inits-hooks' is run, so the time taken to
run those hooks is included.")

;; Defined in v23
(defvar before-init-time nil)
(defvar after-init-time nil)


;;; This magically puts bytecode from my $sinit/emacs init files and my
;;; elisp source trees into the right place.

;; ----------
(defun-compile init-bcf-byte-compile-dest-file-table ()
  (bcf-byte-compile-set-dest-file
    (lambda (name)
      (let* ((dir1 (expand-file-name (getenv "sinit")))
             (dir2 (expand-file-name "~/lib/elisp/init/"))
             (dir3 (expand-file-name "~/lib/elisp/domain/site-init/"))
             (re (format "^%s/\\(local/\\)?emacs/\\|^%s\\|^%s"
                         (regexp-quote dir1)
                         (regexp-quote dir2)
                         (regexp-quote dir3))))
        (string-match re (expand-file-name name))))
    (lambda (name)
      (expand-file-name
       (format "~/lib/elisp/%s%dc/.init/%sc"
               (emacs-version-prefix)
               (emacs-version-major)
               (file-name-nondirectory name)))))

  (bcf-byte-compile-set-dest-file
    (lambda (name)
      (let* ((dir (expand-file-name "~/lib/elisp/"))
             (subdirs (list (user-login-name)
                            "noahf"
                            "common"
                            "domain"
                            (format "%s%s"
                                    (emacs-version-prefix)
                                    (emacs-version-major))))
             (re-subdirs (mapconcat 'identity subdirs "\\|"))
             (re (format "^%s\\(%s\\)/" dir re-subdirs)))
        (string-match re (expand-file-name name))))
    (lambda (name)
      (setq name (expand-file-name name))
      (expand-file-name

       (format "~/lib/elisp/%s%dc/%sc"
               (emacs-version-prefix)
               (emacs-version-major)
               ;;(substring name (match-end 0))
               (file-name-nondirectory name))))))


;; ----------
(put 'add-after-load-libraries 'lisp-indent-function 0)
(defun-compile add-after-load-libraries (&rest libs)
  "Add LIBS to list of libraries to load after else is done in `do-inits'."
  (while libs
    (or (member (car libs) after-load-libraries)
        (setq after-load-libraries (cons (car libs) after-load-libraries)))
    (setq libs (cdr libs))))

;; ----------
(put 'remove-after-load-libraries 'lisp-indent-function 0)
(defun-compile remove-after-load-libraries (&rest libs)
  (while libs
    (setq after-load-libraries (delete (car libs) after-load-libraries))
    (setq libs (cdr libs))))

;; ----------
(defun-compile emacs-version-prefix ()
  "Return a prefix string which indicates emacs type.
This is used to construct names for files or directories that contain code
specific to some particular variant of emacs (see `emacs-variant').
For the original GNU Emacs, this string is empty.  For all others it is the
variant type followed by a hyphen, e.g. \"xemacs-\"."
  (let ((variant (emacs-variant)))
    (if (eq variant 'emacs)
        ""
      (concat (symbol-name variant) "-"))))


;; ----------
(defun-compile do-inits ()
  (interactive)
  (setq before-do-inits-time (current-time))
  (setq debug-on-error t)

  (or (getenv "sinit")
      (setenv "sinit" (expand-file-name "~/etc/init")))

  (let* ((sinit (getenv "sinit"))
         (prefix (emacs-version-prefix))
         (major (emacs-version-major))
         (variant-str (format "%s%d" prefix major))

         ;; These should be in reverse order since they are consed onto the
         ;; front.
         (dirs (list "~/lib/elisp/common"
                     "~/lib/elisp/noahf"
                     "~/lib/elisp/init"
                     (format "%s/emacs"              sinit)
                     (format "%s/local/emacs"        sinit)
                     (format "~/lib/elisp/%s"        variant-str)
                     (format "~/lib/elisp/%sc"       variant-str)
                     (format "~/lib/elisp/%sc/.init" variant-str))))
    (while dirs
      (setq load-path (cons (expand-file-name (car dirs)) load-path))
      (setq dirs (cdr dirs)))

    (and (> (emacs-version-major) 18)
         (init-bcf-byte-compile-dest-file-table))

    (setq load-offer-compile-dynamic-p               t
          load-offer-compile-default-action          'compile-and-load
          load-offer-compile-check-directory-mkdir-p nil)

    ;; Reload these to get the compiled versions
    (load-offer-compile "load-fns")
    (load-offer-compile "bytecomp-fns")

    (let ((n major))
      ;; Attempt to load the latest init file for this version of emacs.
      ;; This doesn't load all available versions, just the single latest one
      ;; which can be found.  It's up to each init file to load earlier ones,
      ;; if appropriate.
      (while (and (not (load-offer-compile (format "init-%s%d" prefix n) t))
                  (> n 18))
        (setq n (1- n))))

    ;; Load the init-*-only file for just this version of emacs, if there is one.
    (load-offer-compile (format "init-%s%d-only" prefix major) t)

    ;; Load the most domain-specific site config file for a domain, if any
    ;; exist.  Any more general site config files must be loaded by the most
    ;; specific one found; this provides the most domain-specific control.
    ;; This rule is followed for each domain in SINITDOMAIN; that is, if
    ;; domains internal.foo.com and bar.com are defined and internal.foo.com
    ;; is read, foo.com will not be implicitly read but bar.com will be.
    (require-offer-compile 'string-fns)
    (let ((domains (if (getenv "SINITDOMAIN")
                       (string-split (getenv "SINITDOMAIN") ":")
                     (list (system-name))))
          parts)
      (while domains
        (setq parts (string-split (downcase (car domains)) "\\.")
              domains (cdr domains))
        (while parts
          (if (load-offer-compile (mapconcat 'identity (reverse parts) ".") t)
              (setq parts nil)
            (setq parts (cdr parts))))))

    ;; One of the init files might already have set it
    ;; to something else (e.g. a list of error
    ;; conditions).  If so, don't reset it.
    (and (eq t debug-on-error)
         (setq debug-on-error nil))

    ;; Load all deferred libraries in their originally-specified order.
    (and after-load-libraries
         (apply 'load-libraries-with-debugging-if-exist
                (reverse after-load-libraries)))

    ;; Load terminal-specific stuff if we're on a terminal.
    (or window-system
        (load-offer-compile (concat "term/" (getenv "TERM")) t))

    (run-hooks 'after-do-inits-hook)
    (setq after-do-inits-time (current-time))

    (when (>= (emacs-version-major) 23)
      (let* ((e-elapsed (float-time (subtract-time after-init-time
                                                   before-init-time)))
             (d-elapsed (float-time (subtract-time after-do-inits-time
                                                   before-do-inits-time)))
             (t-elapsed (+ e-elapsed d-elapsed)))
        (if (< t-elapsed 60)
            (message "Initialization completed in %.2fs (%.2f + %.2f)"
                     t-elapsed e-elapsed d-elapsed)
          (message "Initialization completed in %s"
                   (format-seconds "%mm%ss" t-elapsed)))))))


;; Set some variables here that must be done before after-init-hook is called.
;;
;; Note that the contents of after-init-hook (which loads all the real init
;; stuff) is run AFTER default.el is loaded.
(setq inhibit-startup-message         t
      initial-major-mode              'lisp-interaction-mode
      mode-line-inverse-video         nil
      after-init-hook                 'do-inits)

;; Version 18 doesn't have after-init-hook, so we have to do this kludge to
;; get the desired effect.
;; Since emacs 18 can't really handle many of my libraries anymore, don't
;; do this automatically.
;(and (= (emacs-version-major) 18)
;     (setq term-setup-hook after-init-hook))

(provide 'init)

;;; init.el ends here.
