;;; init-solar.el

;; Copyright (C) 1994, 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1994-10-26

;; $Id: init-solar.el,v 1.6 2008/09/01 23:17:21 friedman Exp $

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

;; This file contains expressions and variables that are loaded and
;; evaluated after loading solar.el.

;;; Code:

(require 'solar)

(setq calendar-time-display-form
      '(24-hours ":" minutes (and time-zone " ") time-zone))

(defvar init-solar-coordinate-alist
  '((cambridge ;; zip 02139
     (calendar-location-name           "Cambridge, MA")
     (calendar-standard-time-zone-name "EST")
     (calendar-daylight-time-zone-name "EDT")
     (calendar-time-zone               (* -5 60))
     (calendar-longitude               [71 07 west])
     (calendar-latitude                [42 22 north]))

    (austin ;; zip 78705
     (calendar-location-name           "Austin, TX")
     (calendar-standard-time-zone-name "CST")
     (calendar-daylight-time-zone-name "CDT")
     (calendar-time-zone               (* -6 60))
     (calendar-longitude               [97 44 west])
     (calendar-latitude                [30 16 north]))

    (mountain-view ;; zip 94043
     (calendar-location-name           "Mountain View, CA")
     (calendar-standard-time-zone-name "PST")
     (calendar-daylight-time-zone-name "PDT")
     (calendar-time-zone               (* -8 60))
     (calendar-longitude               [122 04 west])
     (calendar-latitude                [ 37 15 north]))

    (san-francisco
     (calendar-location-name           "San Francisco, CA")
     (calendar-standard-time-zone-name "PST")
     (calendar-daylight-time-zone-name "PDT")
     (calendar-time-zone               (* -8 60))
     (calendar-longitude               [122 15 west])
     (calendar-latitude                [ 37 27 north]))

    (oakland
     (calendar-location-name           "Oakland, CA")
     (calendar-standard-time-zone-name "PST")
     (calendar-daylight-time-zone-name "PDT")
     (calendar-time-zone               (* -8 60))
     (calendar-longitude               [122 15 west])
     (calendar-latitude                [ 37 27 north]))))

;; 37.828157 N, 122.22683 W


;; Decide which coordinates to give based on where I appear to be
;; timezone-wise.
;; This isn't perfect, since my latitude might differ, but these are where
;; I am most of the time.
;; (let ((zn (car (cdr (current-time-zone)))))
;;   (cond
;;    ((member zn '("EST" "EDT"))
;;     (set-solar-coordinates:cambridge))
;;    ((member zn '("CST" "CDT"))
;;     (set-solar-coordinates:austin))
;;    ((member zn '("PST" "PDT"))
;;     (set-solar-coordinates:san-francisco))))

(provide 'init-solar)

;;; init-solar.el ends here.
