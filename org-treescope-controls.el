;;; org-treescope-controls.el --- Controls for org-treescope package -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3"))
;; Version: 0.1

;;; Commentary:

;; See org-treescope.el

;;; Code:

(require 'newlib)

(defvar newlib--day--leftflank nil)
(defvar newlib--day--rightflank nil)
(defvar newlib--day--frommidpoint-select nil "Possible values are `<=` and `>=`.")

;; -- Macros
(defmacro newlib--defaults-and-updates (&rest innercode)
  "Set default NDAYS to 1 and silent to true, run INNERCODE, and then update-now."
  `(let ((ndays (if ndays ndays 1)))
     ,@innercode
     (unless silent
       (newlib--sensible-values)
       (newlib--constructformat))))

(defmacro newlib--shift-ranges (direction lowerb upperb)
  "Call the LOWERB and UPPERB (low/up bounds) in DIRECTION.
Reset the `newlib--day--frommidpoint-select' to nil."
  `(newlib--defaults-and-updates
    (,lowerb ndays t)
    (calendar-forward-day (,direction ndays))
    (,upperb ndays t)))

(defmacro newlib--shift-flanks (day-flank positive)
  "Shift either the DAY-FLANK (left or right) flank in a POSITIVE or negative direction."
  ;; Correctly drags the midpoint.
  `(newlib--defaults-and-updates
    (let ((midpoint (newlib--getmidpoint-abs)))
      (setq ,day-flank (,positive ,day-flank ndays))
      (if (or (< midpoint newlib--day--leftflank)
              (> midpoint newlib--day--rightflank))
          (calendar-cursor-to-visible-date (calendar-gregorian-from-absolute ,day-flank))))))


;; -- Date Methods
(defun newlib-day-shiftrange-backwards (&optional ndays silent)
  "Shift entire range back by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (newlib--shift-ranges - newlib-day-lowerbound-backwards newlib-day-upperbound-backwards))

(defun newlib-day-shiftrange-backwards-week (&optional silent)
  "Shift entire range back by a week and update midpoint.  Don't update if SILENT."
  (interactive)
  (newlib-day-shiftrange-backwards 7 silent))

(defun newlib-day-shiftrange-forwards-week (&optional silent)
  "Shift entire range forwards by a week and update midpoint.  Don't update if SILENT."
  (interactive)
  ;; FIXME: why doesn't (newlib-day-shiftrange-forwards 7 t) work reliably?
  ;;       - it seems any number over 3 does not jump to where it should,
  ;;       - does not seem to be related to the sensible-values mid 3 thing
  (newlib-day-shiftrange-forwards 3 t)
  (newlib-day-shiftrange-forwards 3 t)
  (newlib-day-shiftrange-forwards 1 t))

(defun newlib-day-shiftrange-forwards (&optional ndays silent)
  "Shift entire range forwards by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (newlib--shift-ranges + newlib-day-lowerbound-forwards newlib-day-upperbound-forwards))

(defun newlib-day-lowerbound-forwards (&optional ndays silent)
  "Move left-flank by NDAYS forwards.  Don't update if SILENT."
  (interactive)
  (newlib--shift-flanks newlib--day--leftflank +))

(defun newlib-day-lowerbound-backwards (&optional ndays silent)
  "Move left-flank by NDAYS backwards.  Don't update if SILENT."
  (interactive)
  (newlib--shift-flanks newlib--day--leftflank -))

(defun newlib-day-upperbound-forwards (&optional ndays silent)
  "Move right-flank by NDAYS forwards.  Don't update if SILENT."
  (interactive)
  (newlib--shift-flanks newlib--day--rightflank +))

(defun newlib-day-upperbound-backwards (&optional ndays silent)
  "Move right-flank by NDAYS backwards.  Don't update if SILENT."
  (interactive)
  (newlib--shift-flanks newlib--day--rightflank -))

(defun newlib-day-frommidpoint-leftwards (&optional silent)
  "Ignore left and right flanks, and select all dates before midpoint.  Don't update if SILENT."
  (interactive)
  (let ((ndays nil))
    (newlib--defaults-and-updates (setq newlib--day--frommidpoint-select "<="))))

(defun newlib-day-frommidpoint-rightwards (&optional silent)
  "Ignore left and right flanks, and select all dates after midpoint.  Don't update if SILENT."
  (interactive)
  (let ((ndays nil))
    (newlib--defaults-and-updates (setq newlib--day--frommidpoint-select ">="))))

(defun newlib-day-frommidpoint-stop (&optional silent)
  "Set the flank selector to nothing and restore shift range mode.  Don't update if SILENT."
  (interactive)
  (setq newlib--day--frommidpoint-select nil)
  (unless silent (newlib--constructformat)))

;; Attempt to macrofy interactive functions, does not save lines

;; (defmacro newlib-macro-daybound (islow isfwd)
;;   "Make interactive functions to move individual flanks, with ISLOW and ISFWD."
;;   (let ((prefix "newlib")
;;         (boundtype (if islow "lowerbound" "upperbound"))
;;         (direction (if isfwd "forwards" "backwards"))
;;         (funcdirec (if isfwd "+" "-"))
;;         (flank (if isfwd "leftflank" "rightflank")))
;;     (let ((funcname (intern (format "%s-day-%s-%s" prefix boundtype direction)))
;;           (funcdocs (format "Move %s by NDAYS %s.  Don't update if SILENT." flank direction))
;;           (funcflnk (intern (format "%s--shift-flanks" prefix)))
;;           (funcbody (intern (format "%s--day--%s" prefix flank)))
;;           (funcdirc (intern funcdirec)))
;;       `(defun ,funcname (&optional ndays silent)
;;          ,funcdocs
;;          (interactive)
;;          (,funcflnk ,funcbody ,funcdirc)))))

;; (newlib-macro-daybound t t) ;; lowerbound forwards
;; (newlib-macro-daybound t nil) ;; lowerbound backwards
;; (newlib-macro-daybound nil t) ;; upperbound forwards
;; (newlib-macro-daybound nil nil) ;; upperbound backwards

(provide 'newlib-controls)

;;; org-treescope-controls.el ends here