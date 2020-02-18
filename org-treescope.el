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
  (setq day--leftflank nil
        day--rightflank nil
        day--midpoint nil
        day--frommidpoint-select nil)
  (sensible-values)
  (update-all))

(defun sensible-values ()
  "Checks that all four defvars are initialised and at sensible defaults."
  ;; We deal with absolute dates, not gregorian.
  (unless day--midpoint
    (setq day--midpoint
          (calendar-absolute-from-gregorian
           (calendar-current-date))))
  (unless day--leftflank (setq day--leftflank (- day--midpoint 3)))
  (unless day--rightflank (setq day--rightflank (+ day--midpoint 3)))
  ;; -- check sensible values --
  (if (> day--leftflank day--rightflank)
      (setq day--rightflank (+ day--leftflank 1)))
  (if (< day--rightflank day--leftflank)
      (setq day--leftflank (- day--rightflank 1))))
  ;; TODO: Add clauses for what the midpoint is doing


;; -- Date Macros
(defmacro defaults-and-updates (&rest innercode)
  "Set default ndays to 1 and updatenow to true, run INNERCODE, and then update-now"
  `(let ((ndays (or 1 ndays))
         (updatenow (not (or nil updatenow))))
     (progn ,@innercode
            (sensible-values))
     (if updatenow (update-all))))

(defmacro shift-ranges (positive lowerb upperb)
  "Call the lowerbound and upperbound with POSITIVE or negative.
Reset the `day--frommidpoint-select` to nil."
  `(defaults-and-updates
     (,lowerb ndays nil)
     (,upperb ndays nil)
     (setq day--midpoint (,positive day--midpoint ndays))
     (setq day--frommidpoint-select nil)))

(defmacro shift-flanks (day-flank positive)
  "Shift either the TYPE (left or right) flank in a POSITIVE or negative direction"
  `(defaults-and-updates
     (setq ,day-flank (,positive ,day-flank ndays))
     (if (or (< day--midpoint day--leftflank)
             (> day--midpoint day--rightflank))
         (setq day--midpoint ,day-flank))))

;; -- Date Methods
(defun day-shiftrange-backwards (&optional ndays updatenow)
  "Shift entire range back by NDAYS and update midpoint.  Redraw if UPDATENOW."
  (interactive)
  (shift-ranges - day-lowerbound-backwards day-upperbound-backwards))

(defun day-shiftrange-forwards (&optional ndays updatenow)
  "Shift entire range forwards by NDAYS and update midpoint.  Redraw if UPDATENOW."
  (interactive)
  (shift-ranges + day-lowerbound-forwards day-upperbound-forwards))

(defun day-lowerbound-forwards (&optional ndays updatenow)
  "Move left-flank by NDAYS forwards.  Redraw if UPDATENOW."
  (interactive)
  (shift-flanks day--leftflank +))

(defun day-lowerbound-backwards (&optional ndays updatenow)
  "Move left-flank by NDAYS backwards.  Redraw if UPDATENOW."
  (interactive)
  (shift-flanks day--leftflank -))

(defun day-upperbound-forwards (&optional ndays updatenow)
  "Move right-flank by NDAYS forwards.  Redraw if UPDATENOW."
  (interactive)
  (shift-flanks day--rightflank +))

(defun day-upperbound-backwards (&optional ndays updatenow)
  "Move right-flank by NDAYS backwards.  Redraw if UPDATENOW."
  (interactive)
  (shift-flanks day--rightflank -))

(defun day-frommidpoint-leftwards (&optional updatenow)
  "Ignore left and right flanks, and select all dates before midpoint.  Redraw if UPDATENOW."
  (interactive)
  (defaults-and-updates (setq day--frommidpoint-select "<=")))

(defun day-frommidpoint-rightwards (&optional updatenow)
  "Ignore left and right flanks, and select all dates after midpoint.  Redraw if UPDATENOW."
  (interactive)
  (defaults-and-updates (setq day--frommidpoint-select ">=")))

;; -- Update method --
(defun update-datestring ()
  "Update the date string based on current state."
  (let ((format-lambda '(lambda (x) (format "%s" x))))
    (if day--frommidpoint-select
        (let* ((gregdate-mid (calendar-gregorian-from-absolute day--midpoint))
               (strdate-mid (mapconcat format-lambda (reverse gregdate-mid) "-")))
          ;; e.g. <=<2020-12-02> or >=<2019-01-31>
          (setq state-times (format "TIMESTAMP%s<%s>" day--frommidpoint-select strdate-mid)))
      ;; Otherwise set a date range.
      (let ((gregdate-left  (calendar-gregorian-from-absolute day--leftflank))
            (gregdate-right (calendar-gregorian-from-absolute day--rightflank)))
        (let ((strdate-left (mapconcat format-lambda (reverse gregdate-left) "-"))
              (strdate-right (mapconcat format-lambda (reverse gregdate-right) "-")))
          (setq state-times (format "TIMESTAMP>=<%s>&TIMESTAMP<=<%s>" strdate-left strdate-right)))))
    (message state-times))
  ;; For some reason shift-ranges does not parse it unless I put it here
  (setq day--frommidpoint-select nil))


;; --- Todos and Priorities ---
(defvar state-times nil "Final form string component of TIME range")
(defvar state-todos nil "Final form string component of TODO states")
(defvar state-priority nil "Final form string component of PRIORITY states")


(defvar todogroups-state nil  "Current state of TODO custom group.")
(defcustom todogroups
  '(nil ("DONE") ("TODO" "DOING") ("TODO" "DONE") ("WAITING"))
  "List of TODO groups to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'treescope)

(defvar prioritygroups-state nil  "Current state of GROUP custom group.")
(defcustom prioritygroups
  '(nil (65 68) (65 70) (70 75))
  "List of PRIORITY ranges (lowest highest) to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'treescope)

(defmacro next-state (statecurrent statelist &rest rest)
  "Set the next state in the STATELIST from the STATECURRENT."
  `(let* ((now-index (or (position ,statecurrent ,statelist :test 'equal) 0))
          (nxt-index (mod (+ 1 now-index) (length ,statelist))))
     (let ((nxt-state (nth nxt-index ,statelist)))
       (setq ,statecurrent nxt-state)
       ,@rest)))

(defun cycle-todo-states ()
  "Cycle the TODO groups given by the `todogroups` variable."
  (interactive)
  (next-state
   todogroups-state todogroups
   (if nxt-state
       (let* ((string-fmt (mapconcat 'identity nxt-state "\\|"))
              (string-out (format "TODO={%s}" string-fmt)))
         (setq state-todos string-out))
     (setq state-todos nil))))

(defun cycle-priority-states ()
  "Cycle the PRIORITY groups given by the `todogroups` variable."
  (interactive)
  (next-state
   prioritygroups-state prioritygroups
   (if nxt-state
       (let ((string-out `(format "PRIORITY>=%s&PRIORITY<=%s" ,@prioritygroups-state)))
         (setq state-priority (eval string-out)))
     (setq state-priority nil))))

;; -- Calendar Functions
(defun calendar-isopen ()
  "True if calendar is showing"
  (member "*Calendar*"
          (--map (buffer-name (window-buffer it)) (window-list))))

(defmacro markdate (abs face)
  `(calendar-mark-visible-date (calendar-gregorian-from-absolute ,abs) ,face))

(defun update-calendar ()
  "Show and update the calendar to show the left, right, and middle flanks."
  (let ((cb (current-buffer)))
    (unless (calendar-isopen)
      (calendar))
    ;; (get-buffer-window cb)
    (mode6 t)
    (calendar-unmark)
    (dolist (absdate (number-sequence day--leftflank day--rightflank))
      (cond
       ((eq absdate day--midpoint) (markdate day--midpoint midday-marker))
       (t (markdate absdate range-marker))))))

(defun update-all (&optional silent)
  "Update the datestring and show on calendar."
  (update-datestring)
  (if (not silent) (update-calendar)))


(define-minor-mode mode6
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
    ([down] . reset-values)
    ([return] . mode6)))

;; -- Faces --
(defface marker-range
  '((((class color) (background light))
     :background "darkblue")
    (((class color) (background dark))
     :background "darkblue")
    (t :inverse-video t))
  "Face for showing the range markers."
  :group 'treescope-faces)

(defface marker-midday
  '((((class color) (background light))
     :background "green")
    (((class color) (background dark))
     :background "green")
    (t :inverse-video t))
  "Face for showing the middle marker."
  :group 'treescope-faces)

(defcustom range-marker 'marker-range
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'treescope)

(defcustom midday-marker 'marker-midday
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'treescope)

(provide 'org-treescope)
;;; org-treescope.el ends here
