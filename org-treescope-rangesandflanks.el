;;; org-treescope-rangesandflanks.el --- Moving and modifying the calendar range -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3") (org-ql "0.5-pre"))
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
(defvar org-treescope--day--leftflank nil)
(defvar org-treescope--day--rightflank nil)
(defvar org-treescope--day--frommidpoint-select nil "Possible values are `:to' and `:from'.")

(defmacro org-treescope--defaults-and-updates (&rest innercode)
  "Set default NDAYS to 1 and silent to true, run INNERCODE, and then update-now."
  `(let ((ndays (if ndays ndays 1)))
     ,@innercode
     (unless silent
       (org-treescope--sensible-values)
       (org-treescope-apply-to-buffer))))

(defmacro org-treescope--shift-ranges (direction lowerb upperb)
  "Call the LOWERB and UPPERB (low/up bounds) in DIRECTION.
Reset the `org-treescope--day--frommidpoint-select' to nil."
  `(org-treescope--defaults-and-updates
    (,lowerb ndays t)
    (calendar-forward-day (,direction ndays))
    (,upperb ndays t)))

(defmacro org-treescope--shift-flanks (day-flank positive)
  "Shift either the DAY-FLANK (left or right) flank in a POSITIVE or negative direction."
  ;; Correctly drags the midpoint.
  `(org-treescope--defaults-and-updates
    (let ((midpoint (org-treescope--getmidpoint-abs)))
      (setq ,day-flank (,positive ,day-flank ndays))
      (if (or (< midpoint org-treescope--day--leftflank)
              (> midpoint org-treescope--day--rightflank))
          (calendar-cursor-to-visible-date (calendar-gregorian-from-absolute ,day-flank))))))

(defun org-treescope--sensible-values ()
  "Check that all time flankers are initialised and at sensible defaults."
  ;; We deal with absolute dates, not gregorian.
  (let ((mid (org-treescope--getmidpoint-abs)))
    (unless org-treescope--day--leftflank (setq org-treescope--day--leftflank (- mid 3)))
    (unless org-treescope--day--rightflank (setq org-treescope--day--rightflank (+ mid 3))))
  ;; -- check sensible values --
  ;; left outflanks right
  (if (> org-treescope--day--leftflank org-treescope--day--rightflank)
      (setq org-treescope--day--rightflank
            (+ org-treescope--day--leftflank 1)))
  ;; right outflanks left
  (if (< org-treescope--day--rightflank org-treescope--day--leftflank)
      (setq org-treescope--day--leftflank
            (- org-treescope--day--rightflank 1))))

;; -- Date Methods
;;;###autoload
(defun org-treescope-day-lowerbound-forwards (&optional ndays silent)
  "Move left-flank by NDAYS forwards.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-flanks org-treescope--day--leftflank +))

;;;###autoload
(defun org-treescope-day-lowerbound-backwards (&optional ndays silent)
  "Move left-flank by NDAYS backwards.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-flanks org-treescope--day--leftflank -))

;;;###autoload
(defun org-treescope-day-upperbound-forwards (&optional ndays silent)
  "Move right-flank by NDAYS forwards.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-flanks org-treescope--day--rightflank +))

;;;###autoload
(defun org-treescope-day-upperbound-backwards (&optional ndays silent)
  "Move right-flank by NDAYS backwards.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-flanks org-treescope--day--rightflank -))

;;;###autoload
(defun org-treescope-day-frommidpoint-leftwards (&optional ndays silent)
  "Ignore left and right flanks, and select all dates before midpoint.
Don't update if SILENT.  NDAYS exists for macro purposes."
  (interactive)
  (org-treescope--defaults-and-updates (ignore ndays) (setq org-treescope--day--frommidpoint-select :to)))

;;;###autoload
(defun org-treescope-day-frommidpoint-rightwards (&optional ndays silent)
  "Ignore left and right flanks, and select all dates after midpoint.
Don't update if SILENT.  NDAYS exists for macro purposes."
  (interactive)
  (org-treescope--defaults-and-updates (ignore ndays) (setq org-treescope--day--frommidpoint-select :from)))

;;;###autoload
(defun org-treescope-day-frommidpoint-stop (&optional silent)
  "Set the flank selector to nothing and restore shift range mode.  Don't update if SILENT."
  (interactive)
  (setq org-treescope--day--frommidpoint-select nil)
  (unless silent (org-treescope-apply-to-buffer)))

;;;###autoload
(defun org-treescope-day-shiftrange-backwards (&optional ndays silent)
  "Shift entire range back by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-ranges - org-treescope-day-lowerbound-backwards org-treescope-day-upperbound-backwards))

;;;###autoload
(defun org-treescope-day-shiftrange-forwards (&optional ndays silent)
  "Shift entire range forwards by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-ranges + org-treescope-day-lowerbound-forwards org-treescope-day-upperbound-forwards))

;;;###autoload
(defun org-treescope-day-shiftrange-backwards-week (&optional silent)
  "Shift entire range back by a week and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope-day-shiftrange-backwards 7 silent))

;;;###autoload
(defun org-treescope-day-shiftrange-forwards-week (&optional silent)
  "Shift entire range forwards by a week and update midpoint.  Don't update if SILENT."
  (interactive)
  (ignore silent)
  ;; FIXME: why doesn't (org-treescope-day-shiftrange-forwards 7 t) work reliably?
  ;;       - it seems any number over 3 does not jump to where it should,
  ;;       - does not seem to be related to the sensible-values mid 3 thing
  ;;       - it seems like the calendar cursor is not set properly
  (org-treescope-day-shiftrange-forwards 3 t)
  (org-treescope-day-shiftrange-forwards 3 t)
  (org-treescope-day-shiftrange-forwards 1 nil))

(provide 'org-treescope-rangesandflanks)
;;; org-treescope-rangesandflanks.el ends here
