;;; org-treescope-calendarranges.el --- Moving and modifying the calendar range -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3") (org-ql "0.5-pre") (dash "2.17.0"))
;; Version: 0.3

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; see org-treescope.el

;;; Code:
(require 'calendar)
(require 'dash)

(require 'org-treescope-datehelper) ;; brings calendar
(require 'org-treescope-faces) ;; brings nil

(defvar org-treescope-calendarranges--day--leftflank nil)
(defvar org-treescope-calendarranges--day--rightflank nil)
(defvar org-treescope-calendarranges--day--frommidpoint-select nil "Possible values are `:to' and `:from'.")

(defmacro org-treescope-calendarranges--defaults-and-updates (&rest innercode)
  "Set default NDAYS to 1 and silent to true, run INNERCODE, and then update-now."
  `(let ((ndays (if ndays ndays 1)))
     ,@innercode
     (unless silent
       (org-treescope-calendarranges--sensible-values))))

(defmacro org-treescope-calendarranges--shift-ranges (direction lowerb upperb)
  "Call the LOWERB and UPPERB (low/up bounds) in DIRECTION.
Reset the `org-treescope-calendarranges--day--frommidpoint-select' to nil."
  `(org-treescope-calendarranges--defaults-and-updates
    (,lowerb ndays t)
    (calendar-forward-day (,direction ndays))
    (,upperb ndays t)))

(defmacro org-treescope-calendarranges--shift-flanks (day-flank positive)
  "Shift either the DAY-FLANK (left or right) flank in a POSITIVE or negative direction."
  ;; Correctly drags the midpoint.
  `(org-treescope-calendarranges--defaults-and-updates
    (let ((midpoint (org-treescope-datehelper--getmidpoint-abs)))
      (setq ,day-flank (,positive ,day-flank ndays))
      (if (or (< midpoint org-treescope-calendarranges--day--leftflank)
              (> midpoint org-treescope-calendarranges--day--rightflank))
          (calendar-cursor-to-visible-date (calendar-gregorian-from-absolute ,day-flank))))))

(defun org-treescope-calendarranges--sensible-values ()
  "Check that all time flankers are initialised and at sensible defaults."
  ;; We deal with absolute dates, not gregorian.
  (let ((mid (org-treescope-datehelper--getmidpoint-abs)))
    (unless org-treescope-calendarranges--day--leftflank (setq org-treescope-calendarranges--day--leftflank (- mid 3)))
    (unless org-treescope-calendarranges--day--rightflank (setq org-treescope-calendarranges--day--rightflank (+ mid 3))))
  ;; -- check sensible values --
  ;; left outflanks right
  (if (> org-treescope-calendarranges--day--leftflank org-treescope-calendarranges--day--rightflank)
      (setq org-treescope-calendarranges--day--rightflank
            (+ org-treescope-calendarranges--day--leftflank 1)))
  ;; right outflanks left
  (if (< org-treescope-calendarranges--day--rightflank org-treescope-calendarranges--day--leftflank)
      (setq org-treescope-calendarranges--day--leftflank
            (- org-treescope-calendarranges--day--rightflank 1))))

;; -- Date Methods
(defun org-treescope-calendarranges-day--lowerbound-forwards (&optional ndays silent)
  "Move left-flank by NDAYS forwards.  Don't update if SILENT."
  (org-treescope-calendarranges--shift-flanks org-treescope-calendarranges--day--leftflank +))

(defun org-treescope-calendarranges-day--lowerbound-backwards (&optional ndays silent)
  "Move left-flank by NDAYS backwards.  Don't update if SILENT."
  (org-treescope-calendarranges--shift-flanks org-treescope-calendarranges--day--leftflank -))

(defun org-treescope-calendarranges-day--upperbound-forwards (&optional ndays silent)
  "Move right-flank by NDAYS forwards.  Don't update if SILENT."
  (org-treescope-calendarranges--shift-flanks org-treescope-calendarranges--day--rightflank +))

(defun org-treescope-calendarranges-day--upperbound-backwards (&optional ndays silent)
  "Move right-flank by NDAYS backwards.  Don't update if SILENT."
  (org-treescope-calendarranges--shift-flanks org-treescope-calendarranges--day--rightflank -))

(defun org-treescope-calendarranges-day--frommidpoint-leftwards (&optional ndays silent)
  "Ignore left and right flanks, and select all dates before midpoint.
Don't update if SILENT.  NDAYS exists for macro purposes."
  (org-treescope-calendarranges--defaults-and-updates (ignore ndays) (setq org-treescope-calendarranges--day--frommidpoint-select :to)))

(defun org-treescope-calendarranges-day--frommidpoint-rightwards (&optional ndays silent)
  "Ignore left and right flanks, and select all dates after midpoint.
Don't update if SILENT.  NDAYS exists for macro purposes."
  (org-treescope-calendarranges--defaults-and-updates (ignore ndays) (setq org-treescope-calendarranges--day--frommidpoint-select :from)))

(defun org-treescope-calendarranges-day--frommidpoint-stop ()
  "Set the flank selector to nothing and restore shift range mode."
  (setq org-treescope-calendarranges--day--frommidpoint-select nil))

(defun org-treescope-calendarranges-day--shiftrange-backwards (&optional ndays silent)
  "Shift entire range back by NDAYS and update midpoint.  Don't update if SILENT."
  (org-treescope-calendarranges--shift-ranges - org-treescope-calendarranges-day--lowerbound-backwards org-treescope-calendarranges-day--upperbound-backwards))

(defun org-treescope-calendarranges-day--shiftrange-forwards (&optional ndays silent)
  "Shift entire range forwards by NDAYS and update midpoint.  Don't update if SILENT."
  (org-treescope-calendarranges--shift-ranges + org-treescope-calendarranges-day--lowerbound-forwards org-treescope-calendarranges-day--upperbound-forwards))

(defun org-treescope-calendarranges-day--shiftrange-backwards-week (&optional silent)
  "Shift entire range back by a week and update midpoint.  Don't update if SILENT."
  (org-treescope-calendarranges-day--shiftrange-backwards 7 silent))

(defun org-treescope-calendarranges-day--shiftrange-forwards-week ()
  "Shift entire range forwards by a week and update midpoint."
  ;; FIXME: why doesn't (org-treescope-calendarranges-day-shiftrange-forwards 7 t) work reliably?
  ;;       - it seems any number over 3 does not jump to where it should,
  ;;       - does not seem to be related to the sensible-values mid 3 thing
  ;;       - it seems like the calendar cursor is not set properly
  (org-treescope-calendarranges-day--shiftrange-forwards 3 t)
  (org-treescope-calendarranges-day--shiftrange-forwards 3 t)
  (org-treescope-calendarranges-day--shiftrange-forwards 1 nil))

(provide 'org-treescope-calendarranges)
;;; org-treescope-calendarranges.el ends here
