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
(defvar newlib--autoupdate-p t ;; used by toggleautoupdate and construct-format
  "Automatically apply the current format string on every user update.")

(defun newlib-toggleautoupdate ()
  "Toggle the auto-update capability for every user-action."
  (interactive)
  (setq newlib--autoupdate-p (not newlib--autoupdate-p)))

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


(defsubst newlib--datetostring (gregdate) ;; update-datestring
  (let ((revdate (reverse gregdate)))
    (eval `(format "%04d-%02d-%02d" ,@revdate))))

(defun newlib--update-datestring () ;; construct-format
  "Update the date string based on current state."
  (when newlib--timemode
    (if newlib--day--frommidpoint-select
        (let* ((gregdate-mid (calendar-cursor-to-date))
               (strdate-mid (newlib--datetostring gregdate-mid)))
          ;; e.g. <=<2020-12-02> or >=<2019-01-31>
          (format "%s%s\"<%s>\""
                  newlib--timemode
                  newlib--day--frommidpoint-select
                  strdate-mid))
      ;; Otherwise set a date range.
      (let ((gregdate-left  (calendar-gregorian-from-absolute newlib--day--leftflank))
            (gregdate-right (calendar-gregorian-from-absolute newlib--day--rightflank)))
        (let ((strdate-left (newlib--datetostring gregdate-left))
              (strdate-right (newlib--datetostring gregdate-right)))
          (format "%s>=\"<%s>\"&%s<=\"<%s>\""
                  newlib--timemode
                  strdate-left
                  newlib--timemode
                  strdate-right))))))


(provide 'newlib-datehelpers)

;;; org-treescope-datehelpers.el ends here
