;;; init-xemacs-21.el --- startup for XEmacs version 20

;; Copyright (C) 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-xemacs-21.el,v 1.3 2003/11/20 18:49:49 friedman Exp $

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

;;;;;;
;;; Defuns
;;;;;;


(load-offer-compile "init-xemacs-20")


;;;;;;
;;; Mode hooks
;;;;;;


;;;;;;
;;; Variables
;;;;;;


;;;;;;
;;; Key bindings
;;;;;;

(define-key global-map 'backspace 'backward-delete-char-untabify)


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;


;;;;;;
;;; External libraries to load
;;;;;;

;; (add-after-load-libraries nil)

(remove-after-load-libraries
  "kf-frobs"
  )

(provide 'init-xemacs-21)

;;; init-xemacs-21.el ends here.
