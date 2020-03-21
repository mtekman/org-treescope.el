;;; org-treescope-calendarranges.el --- Moving and modifying the calendar range -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.3") (org "9.2.3") (org-ql "0.5-pre") (dash "2.17.0"))
;; Version: 0.5

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
(require 'org-treescope-modehelper)

(defvar org-treescope-calendarranges--day--leftflank nil)
(defvar org-treescope-calendarranges--day--rightflank nil)
(defvar org-treescope-calendarranges--day--frommidpoint-select nil "Possible values are `:to' and `:from'.")

(defun org-treescope-calendarranges--sensible-values ()
  "Check that all time flankers are initialised and at sensible defaults."
  ;; We deal with absolute dates, not gregorian.
  (let ((mid (org-treescope-datehelper--getmidpoint-abs))
        (lflank org-treescope-calendarranges--day--leftflank)
        (rflank org-treescope-calendarranges--day--rightflank))
    ;; If not set, then flank the midpoint 3 days either side
    (unless lflank (setq org-treescope-calendarranges--day--leftflank (- mid 3)))
    (unless rflank (setq org-treescope-calendarranges--day--rightflank (+ mid 3)))
    ;; -- check sensible values --
    (if (> lflank rflank)  ;; left outflanks right
        (setq org-treescope-calendarranges--day--rightflank (1+ lflank)))
    (if (< rflank lflank)  ;; right outflanks left
        (setq org-treescope-calendarranges--day--leftflank (1- rflank)))))

;; -- Date Methods
(defun org-treescope-calendarranges-day-lowerbound-forwards (&optional ndays silent)
  "Move left-flank by NDAYS forwards.  Don't update if SILENT."
  (interactive)
  (let ((ndays (or ndays 1))
        (midpoint (org-treescope-datehelper--getmidpoint-abs))
        (lflank org-treescope-calendarranges--day--leftflank)
        (rflank org-treescope-calendarranges--day--rightflank))
    (setq org-treescope-calendarranges--day--leftflank (+ lflank ndays))
    (if (<= lflank midpoint rflank)
        (calendar-cursor-to-visible-date (calendar-gregorian-from-absolute lflank)))
    (unless silent
      (org-treescope-calendarranges--sensible-values))))

(defun org-treescope-calendarranges-day-lowerbound-backwards (&optional ndays silent)
  "Move left-flank by NDAYS backwards.  Don't update if SILENT."
  (interactive)
  (let ((ndays (or ndays 1))
        (midpoint (org-treescope-datehelper--getmidpoint-abs))
        (lflank org-treescope-calendarranges--day--leftflank)
        (rflank org-treescope-calendarranges--day--rightflank))
    (setq org-treescope-calendarranges--day--leftflank (- lflank ndays))
    (if (<= lflank midpoint rflank)
        (calendar-cursor-to-visible-date (calendar-gregorian-from-absolute lflank)))
    (unless silent
      (org-treescope-calendarranges--sensible-values))))

(defun org-treescope-calendarranges-day-upperbound-forwards (&optional ndays silent)
  "Move right-flank by NDAYS forwards.  Don't update if SILENT."
  (interactive)
  (let ((ndays (or ndays 1))
        (midpoint (org-treescope-datehelper--getmidpoint-abs))
        (lflank org-treescope-calendarranges--day--leftflank)
        (rflank org-treescope-calendarranges--day--rightflank))
    (setq org-treescope-calendarranges--day--rightflank (+ rflank ndays))
    (if (<= lflank midpoint rflank)
        (calendar-cursor-to-visible-date (calendar-gregorian-from-absolute rflank)))
    (unless silent
      (org-treescope-calendarranges--sensible-values))))

(defun org-treescope-calendarranges-day-upperbound-backwards (&optional ndays silent)
  "Move right-flank by NDAYS backwards.  Don't update if SILENT."
  (interactive)
  (let ((ndays (or ndays 1))
        (midpoint (org-treescope-datehelper--getmidpoint-abs))
        (lflank org-treescope-calendarranges--day--leftflank)
        (rflank org-treescope-calendarranges--day--rightflank))
    (setq org-treescope-calendarranges--day--rightflank (- rflank ndays))
    (if (<= lflank midpoint rflank)
        (calendar-cursor-to-visible-date (calendar-gregorian-from-absolute rflank)))
    (unless silent
      (org-treescope-calendarranges--sensible-values))))

(defun org-treescope-calendarranges-day-frommidpoint-leftwards (&optional  silent)
  "Ignore left and right flanks, and select all dates before midpoint.  Don't update if SILENT."
  (interactive)
  (setq org-treescope-calendarranges--day--frommidpoint-select :to)
  (unless silent
    (org-treescope-calendarranges--sensible-values)))

(defun org-treescope-calendarranges-day-frommidpoint-rightwards (&optional silent)
  "Ignore left and right flanks, and select all dates after midpoint.  Don't update if SILENT."
  (interactive)
  (setq org-treescope-calendarranges--day--frommidpoint-select :from)
  (unless silent
    (org-treescope-calendarranges--sensible-values)))

(defun org-treescope-calendarranges-day-frommidpoint-stop ()
  "Set the flank selector to nothing and restore shift range mode."
  (interactive)
  (setq org-treescope-calendarranges--day--frommidpoint-select nil))

(defun org-treescope-calendarranges-day-shiftrange-backwards (&optional ndays silent)
  "Shift entire range back by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (let ((ndays (or ndays 1)))
    (calendar-forward-day (- ndays))
    (org-treescope-calendarranges-day-lowerbound-backwards ndays t)
    (org-treescope-calendarranges-day-upperbound-backwards ndays t))
  (unless silent
    (org-treescope-calendarranges--sensible-values)))

(defun org-treescope-calendarranges-day-shiftrange-forwards (&optional ndays silent)
  "Shift entire range forwards by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (let ((ndays (or ndays 1)))
    (calendar-forward-day (+ ndays))
    (org-treescope-calendarranges-day-lowerbound-forwards ndays t)
    (org-treescope-calendarranges-day-upperbound-forwards ndays t))
  (unless silent
    (org-treescope-calendarranges--sensible-values)))

(defun org-treescope-calendarranges-day-shiftrange-backwards-week (&optional silent)
  "Shift entire range back by a week and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope-calendarranges-day-shiftrange-backwards 7 silent))

(defun org-treescope-calendarranges-day-shiftrange-forwards-week ()
  "Shift entire range forwards by a week and update midpoint."
  (interactive)
  ;; FIXME: why doesn't (org-treescope-calendarranges-day-shiftrange-forwards 7 t) work reliably?
  ;;       - it seems any number over 3 does not jump to where it should,
  ;;       - does not seem to be related to the sensible-values mid 3 thing
  ;;       - it seems like the calendar cursor is not set properly
  (org-treescope-calendarranges-day-shiftrange-forwards 3 t)
  (org-treescope-calendarranges-day-shiftrange-forwards 3 t)
  (org-treescope-calendarranges-day-shiftrange-forwards 1 nil))

;; Add controls to mode
(dolist (pair '(("<left>" . org-treescope-calendarranges-day-shiftrange-backwards)
                ("<right>" . org-treescope-calendarranges-day-shiftrange-forwards)
                ("<up>" . org-treescope-calendarranges-day-shiftrange-backwards-week)
                ("<down>" . org-treescope-calendarranges-day-shiftrange-forwards-week)
                ("C-<left>" . org-treescope-calendarranges-day-lowerbound-backwards)
                ("C-<right>" . org-treescope-calendarranges-day-lowerbound-forwards)
                ("M-<left>" . org-treescope-calendarranges-day-upperbound-backwards)
                ("M-<right>" . org-treescope-calendarranges-day-upperbound-forwards)
                ("C-M-<left>" . org-treescope-calendarranges-day-frommidpoint-leftwards)
                ("C-M-<right>" . org-treescope-calendarranges-day-frommidpoint-rightwards)
                ("C-M-<down>" . org-treescope-calendarranges-day-frommidpoint-stop)))
  (cl-pushnew pair org-treescope-modehelper-list))

(provide 'org-treescope-calendarranges)
;;; org-treescope-calendarranges.el ends here
