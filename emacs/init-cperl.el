;;; init-cperl.el --- initialization for cperl-mode

;; Copyright (C) 1999, 2000, 2001, 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1999-11-11

;; $Id: init-cperl.el,v 1.15 2006/06/23 03:10:20 friedman Exp $

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

(require 'cperl-mode)
(require 'list-fns)
(require 'face-fns)

;;;;;;
;;; Defuns
;;;;;;

;; ----------
(defadvice cperl-imenu--create-perl-index (after sinit:unique-symbols activate)
  "Make all unique symbols completable without any package/class namespace qualifier.

Any symbol which is defined in only one namespace will be made available as
a completion without any namespace qualifier.

Symbols defined in more than one namespace can be completed using the fully
qualified name in both forward and reverse order.  For example the
following completions resolve to the same symbol:

        Frobme::OhYeah::baby   <=>   baby!OhYeah!Frobme
        Frobme::Harder::baby   <=>   baby!Harder!Frobme

Reverse names use `!' instead of `::' as the separator merely to
distinguish them from true perl symbols."
  (let ((hier (cdr (assoc "+Hierarchy+..." ad-return-value))))
    (cond ((null hier))
          ((null (cdr hier))
           ;; Just return first and only package hierarchy subtree.  This
           ;; tree contains symbols with no package qualifier, which we
           ;; don't need since it's the same for all symbols.
           (setq ad-return-value (cdar hier)))
          (t
           (let ((tail ad-return-value)
                 (seen (make-hash-table :test 'equal))
                 (elt nil)
                 name)

             ;; find end of list
             (while (cdr tail)
               (and (stringp (caar tail))
                    (not (consp (cdar tail)))
                    (puthash (caar tail) t seen))
               (setq tail (cdr tail)))

             (while hier
               (setq elt (cdar hier))
               (or (consp elt)
                   (setq elt nil)) ; empty package

               (while elt
                 (setq name (caar elt))
                 (when (or (gethash name seen)
                           (catch 'dup
                             ;; Walk rest of hierarchy.  If a symbol is a
                             ;; dup, it will be put into the hashtable.  If
                             ;; not in the table later, it did not appear
                             ;; in a prior part of the hierarchy.
                             (mapc (lambda (pkg)
                                     (and (consp (cdr pkg)) ; skip empty pkg
                                          (assoc name (cdr pkg))
                                          (throw 'dup t)))
                                   (cdr hier))
                             nil))
                   (puthash name t seen)
                   (let* ((parts (split-string (caar hier) "::"))
                          (suffix (mapconcat 'identity (nreverse parts) "!")))
                     (setq name (concat name "!" suffix))))

                 (nconc tail (list (cons name (cdar elt))))
                 (setq tail (cdr tail)
                       elt (cdr elt)))
               (setq hier (cdr hier))))))))

;; ----------
(defadvice cperl-init-faces (after sinit:icperl-init activate)
  (icperl-initialize-faces))

;; ----------
(defun icperl-initialize-faces ()
  (let ((rich-color (cond ((fboundp 'display-color-cells) ;v21
                           (>= (display-color-cells) 88))
                          ((eq window-system 'x) ;v19-v20
                           (>= (x-display-color-cells) 256))
                          (window-system
                           (eq 'color (cdr (assq 'display-type
                                                 (frame-parameters)))))
                          (t nil))))
    (override-face-attributes 'cperl-array-face
      :foreground "goldenrod"
      :background "black"
      :italic nil
      :bold nil)

    (override-face-attributes 'cperl-hash-face
      :foreground (if rich-color "lightgreen" "green")
      :background "black"
      :italic nil
      :bold (if (or window-system rich-color) nil t))

    (override-face-attributes 'cperl-nonoverridable-face
      :foreground (if rich-color "orchid1" "violet")
      :background "black"
      :italic nil
      :bold (if (or window-system rich-color) nil t))))

;; ----------
(defun icperl-make-cperl-mode-default ()
  "Make cperl-mode the default for editing perl scripts"
  (replace-auto-mode "\\.pl\\'" 'cperl-mode)
  (replace-auto-mode "\\.pm\\'" 'cperl-mode)
  (cond ((boundp 'interpreter-mode-alist)
         (let ((cell))
           (while (setq cell (rassq 'perl-mode interpreter-mode-alist))
             (setcdr cell 'cperl-mode)))))
  (defalias 'perl-mode 'cperl-mode))

;; ----------
(defun icperl-mode-setup ()
  "Stuff to do on cperl-mode-hook."
  ;; cperl-do-auto-fill seems to be thoroughly broken, with or without
  ;; filladapt, especially when handling comments.
  (setq normal-auto-fill-function 'do-auto-fill)
  ;; filling "paragraphs" in a code block?  not bloody likely.
  ;; but cperl-fill-paragraph doesn't handle comments properly anyway;
  ;; just use filladapt for this instead.
  (cperl-define-key "\M-q" 'fill-paragraph)
  (filladapt-mode 1))

;;;;;;
;;; Variables
;;;;;;

(setq cperl-hairy                                t)

(setq cperl-auto-newline                         nil)
(setq cperl-auto-newline-after-colon             nil)
(setq cperl-brace-imaginary-offset               0)
(setq cperl-brace-offset                         0)
(setq cperl-close-paren-offset                  -1)
(setq cperl-comment-column                       0) ; 32
(setq cperl-continued-brace-offset               0)
(setq cperl-continued-statement-offset           2)
(setq cperl-extra-newline-before-brace           t)
(setq cperl-extra-newline-before-brace-multiline t)
(setq cperl-indent-level                         2)
(setq cperl-indent-region-fix-constructs         nil)
(setq cperl-label-offset                        -2)
(setq cperl-tab-always-indent                    t)

;; These variables are affected by cperl-hairy; they must be `null'
;; rather than `nil' if not to be overridden by cperl-hairy.
(setq cperl-electric-lbrace-space               'null)
(setq cperl-electric-parens                     'null)
(setq cperl-electric-keywords                   'null)

;;;;;;
;;; Miscellaneous stuff to do
;;;;;;

(add-hook 'cperl-mode-hook 'icperl-mode-setup)

(modify-syntax-entry ?_ "_" cperl-mode-syntax-table)
(icperl-initialize-faces)
(icperl-make-cperl-mode-default)

(provide 'init-cperl)

;;; init-cperl.el ends here.
