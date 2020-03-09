;;; org-treescope-datehelpers.el --- Controls and customizations for TODO and Priority states -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3"))
;; Version: 0.1

;;; Commentary:

;; See org-treescope.el

;;; Code:
(defsubst newlib--getmidpoint () ;; getmidpoint-abs
  "Grabs the date under cursor (if calendar active), or returns the current date."
  (condition-case err
      (calendar-cursor-to-date nil nil)
    (error
     (ignore err)
     (calendar-current-date))))

(defsubst newlib--getmidpoint-abs () ;; called by sensible-values and update-calendar
  "inline substitution to retrieve the current mid point in epochs."
  (calendar-absolute-from-gregorian (newlib--getmidpoint)))

(defmacro newlib--markdate (abs face) ;; update-calendar 
  "Takes an ABS date and highlight it on the calendar with FACE."
  `(calendar-mark-visible-date (calendar-gregorian-from-absolute ,abs) ,face))

(defun newlib--first-of-lastmonth (&optional date) ;; update-calendar
  "Grab the first day of last month, given by DATE."
  (let* ((mont displayed-month)
         (year displayed-year)
         (newm (- mont 1)))
    (if (> newm 0)
        (list newm 1 year)
      (list 12 1 (- year 1)))))

(defun newlib--last-of-nextmonth (&optional date) ;; update-calendar
  "Grab the last day of next month, given by DATE."
  (let* ((mont displayed-month)
         (year displayed-year)
         (newm (+ mont 1)))
    (if (> newm 12)
        (list 1 31 (+ year 1))
      (list newm (calendar-last-day-of-month newm year) year))))


(provide 'newlib-datehelpers)

;;; org-treescope-datehelpers.el ends here
