;;; init-xemacs-20.el --- startup for XEmacs version 20

;; Copyright (C) 1998 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: init-xemacs-20.el,v 1.2 1999/10/19 07:43:28 friedman Exp $

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


(load-offer-compile "init-xemacs-19")


;;;;;;
;;; Mode hooks
;;;;;;


;;;;;;
;;; Variables
;;;;;;

(setq initial-scratch-message nil)


;;;;;;
;;; Key bindings
;;;;;;


;;;;;;
;;; Miscellaneous stuff to do
;;;;;;


;;;;;;
;;; External libraries to load
;;;;;;

;; (add-after-load-libraries nil)

(provide 'init-xemacs-20)

;;; init-xemacs-20.el ends here.
