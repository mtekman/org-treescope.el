;;; org-treescope.el --- Time scoping sparse trees within org -*- lexical-binding: t; -*-

;; Copright (C) 2019 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3"))
;; Version: 0.1

;;; Commentary:

;; Navigating through an org file to see what needs to be done
;; this week and what was completed last month can be tricky.
;; This tool provides a time window to analyse your org file.

;; + Left/Right :: Shift centre day (and whole range) one day to the left/right
;; + S-Left/Right :: Shift centre day (and whole range) one week to left/right
;; + C-Left/Right :: Shift right flank one day to left/right
;; + M-Left/Right :: Shift left flank one day to left/right
;; + C-M-Left/Right :: All the way left/right from centre day
;; + C-S-Left/Right :: [..]
;; + C-M-S-Left/Right :: [..]
;; +
;; + Up/Down :: Shift priority range one step up/down
;; + S-Up/Down :: Shift priority range five steps up/down
;; + C-Up/Down :: Shift bottom priority flank one step up/down
;; + M-Up/Down :: Shift top priority flank one step up/down
;; + C-M-Up/Down :: All the way top/bottom from centre priority -- what does this mean in terms of the search string?
;; + C-S-Up/Down :: [..]
;; + C-M-S-Up/Down :: [..]

;;; Code:

;; Edit -- progn is really not necessary here, but I have no
;;         idea how to bound multiples

;; -- variables
(defvar day--leftflank nil)
(defvar day--rightflank nil)
(defvar day--midpoint nil)
(defvar day--frommidpoint-select nil
  "Possible values are `<=` and `>=`")

;; -- Init --
(defun reset-values ()
  "Reset all variables and center around current date."
  (interactive)
  (progn
    (setq day--leftflank nil
          day--rightflank nil
          day--midpoint nil
          day--frommidpoint-select nil)
    (sensible-values)
    (update-datestring)))

(defun sensible-values ()
  "Checks that all four defvars are initialised and at sensible defaults."
  (progn
    ;; We deal with absolute dates, not gregorian.
    (unless day--midpoint (setq day--midpoint
                                (calendar-absolute-from-gregorian
                                 (calendar-current-date))))
    (unless day--leftflank (setq day--leftflank (- day--midpoint 3)))
    (unless day--rightflank (setq day--rightflank (+ day--midpoint 3))))
  ;; -- check sensible values --
  (unless (< day--leftflank day--rightflank)
    (setq day--rightflank (+ day--leftflank 1))))

;; -- Date Macros
(defmacro defaults-and-updates (innercode)
  "Set default ndays to 1 and updatenow to true, run INNERCODE, and then update-now"
  `'(let ((ndays (or ndays 1))
          (updatenow (or updatenow t)))
      (sensible-values)
      ,innercode
      (when updatenow (update-datestring))))

(defmacro shift-ranges (positive)
  "Call the lowerbound and upperbound with POSITIVE or negative.
Reset the `day--frommidpoint-select` to nil."
  `(setq day--frommidpoint-select nil)
  `(defaults-and-updates
     (progn
        (day-lowerbound-backwards ndays nil)
        (day-upperbound-backwards ndays nil)
        (setq day--midpoint (,positive day--midpoint ndays)))))

(defmacro shift-flanks (day-flank positive)
  "Shift either the TYPE (left or right) flank in a POSITIVE or negative direction"
  `(unless ndays (setq ndays 1))
  `(unless updatenow (setq updatenow t))
    `(setq ,day-flank (,positive ,day-flank ndays)))

;; -- Date Methods
(defun day-shiftrange-backwards (&optional ndays updatenow)
  "Shift entire range back by NDAYS and update midpoint.  Redraw if UPDATENOW."
  (interactive)
  (shift-ranges -))

(defun day-shiftrange-forwards (&optional ndays updatenow)
  "Shift entire range forwards by NDAYS and update midpoint.  Redraw if UPDATENOW."
  (interactive)
  (shift-ranges + ndays updatenow))

(defun day-lowerbound-backwards (&optional ndays updatenow)
  "Move left-flank back by NDAYS.  Redraw if UPDATENOW."
  (interactive)
  (shift-flanks day--leftflank -))

(defun day-lowerbound-forwards (&optional ndays updatenow)
  "Move left-flank forwards by NDAYS.  Redraw if UPDATENOW."
  (interactive)
  (shift-flanks day--leftflank +))

(defun day-upperbound-backwards (&optional ndays updatenow)
  "Move right-flank back by NDAYS.  Redraw if UPDATENOW."
  (interactive)
  (shift-flanks day--rightflank -))

(defun day-upperbound-forwards (&optional ndays updatenow)
  "Move right-flank forwards by NDAYS.  Redraw if UPDATENOW."
  (interactive)
  (shift-flanks day--rightflank +))

(defun day-frommidpoint-leftwards (&optional updatenow)
  "Ignore left and right flanks, and select all dates before midpoint.  Redraw if UPDATENOW."
  (interactive)
  (defaults-and-updates
    (setq day--frommidpoint-select "<=")))

(defun day-frommidpoint-rightwards (&optional updatenow)
  "Ignore left and right flanks, and select all dates after midpoint.  Redraw if UPDATENOW."
  (interactive)
  (defaults-and-updates
    (setq day--frommidpoint-select ">=")))

;; -- Update method --
(defun update-datestring ()
  "Update the date string based on current state."
  (let ((format-lambda '(lambda (x) (format "%s" x))))
    (if day--frommidpoint-select
        (let* ((gregdate-mid (calendar-gregorian-from-absolute day--midpoint))
               (strdate-mid (mapconcat format-lambda (reverse gregdate-left) "-")))
          ;; e.g. <=<2020-12-02> or >=<2019-01-31>
          (message (format "TIMESTAMP%s<%s>" day--frommidpoint-select strdate-mid)))
      ;; Otherwise set a date range.
      (let ((gregdate-left  (calendar-gregorian-from-absolute day--leftflank))
            (gregdate-right (calendar-gregorian-from-absolute day--rightflank)))
        (let ((strdate-left (mapconcat format-lambda (reverse gregdate-left) "-"))
              (strdate-right (mapconcat format-lambda (reverse gregdate-right) "-")))
          (message (format "TIMESTAMP>=<%s>&TIMESTAMP<=<%s>" strdate-left strdate-right)))))))



(define-minor-mode mode5
  "Test"
  :init-value nil
  :lighter " scope"
  :keymap
  '(([left] . day-shiftrange-backwards)
    ([right] . day-shiftrange-forwards)
    ([C-left] . day-lowerbound-backwards)
    ([C-right] . day-lowerbound-forwards)
    ([M-left] . day-upperbound-backwards)
    ([M-right] . day-upperbound-forwards)
    ([C-M-left] . day-frommidpoint-leftwards)
    ([C-M-right] . day-frommidpoint-rightwards)
    ([down] . reset-values)))


(provide 'org-treescope)
;;; org-treescope.el ends here
