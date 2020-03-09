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

(require 'org-treescope)

(defvar org-treescope--day--leftflank nil)
(defvar org-treescope--day--rightflank nil)
(defvar org-treescope--day--frommidpoint-select nil "Possible values are `<=` and `>=`.")

;; -- Macros
(defmacro org-treescope--defaults-and-updates (&rest innercode)
  "Set default NDAYS to 1 and silent to true, run INNERCODE, and then update-now."
  `(let ((ndays (if ndays ndays 1)))
     ,@innercode
     (unless silent
       (org-treescope--sensible-values)
       (org-treescope--constructformat))))

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


;; -- Date Methods
(defun org-treescope-day-shiftrange-backwards (&optional ndays silent)
  "Shift entire range back by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-ranges - org-treescope-day-lowerbound-backwards org-treescope-day-upperbound-backwards))

(defun org-treescope-day-shiftrange-backwards-week (&optional silent)
  "Shift entire range back by a week and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope-day-shiftrange-backwards 7 silent))

(defun org-treescope-day-shiftrange-forwards-week (&optional silent)
  "Shift entire range forwards by a week and update midpoint.  Don't update if SILENT."
  (interactive)
  ;; FIXME: why doesn't (org-treescope-day-shiftrange-forwards 7 t) work reliably?
  ;;       - it seems any number over 3 does not jump to where it should,
  ;;       - does not seem to be related to the sensible-values mid 3 thing
  (org-treescope-day-shiftrange-forwards 3 t)
  (org-treescope-day-shiftrange-forwards 3 t)
  (org-treescope-day-shiftrange-forwards 1 t))

(defun org-treescope-day-shiftrange-forwards (&optional ndays silent)
  "Shift entire range forwards by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-ranges + org-treescope-day-lowerbound-forwards org-treescope-day-upperbound-forwards))

(defun org-treescope-day-lowerbound-forwards (&optional ndays silent)
  "Move left-flank by NDAYS forwards.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-flanks org-treescope--day--leftflank +))

(defun org-treescope-day-lowerbound-backwards (&optional ndays silent)
  "Move left-flank by NDAYS backwards.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-flanks org-treescope--day--leftflank -))

(defun org-treescope-day-upperbound-forwards (&optional ndays silent)
  "Move right-flank by NDAYS forwards.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-flanks org-treescope--day--rightflank +))

(defun org-treescope-day-upperbound-backwards (&optional ndays silent)
  "Move right-flank by NDAYS backwards.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-flanks org-treescope--day--rightflank -))

(defun org-treescope-day-frommidpoint-leftwards (&optional silent)
  "Ignore left and right flanks, and select all dates before midpoint.  Don't update if SILENT."
  (interactive)
  (let ((ndays nil))
    (org-treescope--defaults-and-updates (setq org-treescope--day--frommidpoint-select "<="))))

(defun org-treescope-day-frommidpoint-rightwards (&optional silent)
  "Ignore left and right flanks, and select all dates after midpoint.  Don't update if SILENT."
  (interactive)
  (let ((ndays nil))
    (org-treescope--defaults-and-updates (setq org-treescope--day--frommidpoint-select ">="))))

(defun org-treescope-day-frommidpoint-stop (&optional silent)
  "Set the flank selector to nothing and restore shift range mode.  Don't update if SILENT."
  (interactive)
  (setq org-treescope--day--frommidpoint-select nil)
  (unless silent (org-treescope--constructformat)))

;; Attempt to macrofy interactive functions, does not save lines

;; (defmacro org-treescope-macro-daybound (islow isfwd)
;;   "Make interactive functions to move individual flanks, with ISLOW and ISFWD."
;;   (let ((prefix "org-treescope")
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

;; (org-treescope-macro-daybound t t) ;; lowerbound forwards
;; (org-treescope-macro-daybound t nil) ;; lowerbound backwards
;; (org-treescope-macro-daybound nil t) ;; upperbound forwards
;; (org-treescope-macro-daybound nil nil) ;; upperbound backwards

(provide 'org-treescope-controls)

;;; org-treescope-controls.el ends here
