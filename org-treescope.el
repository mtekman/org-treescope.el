;;; org-treescope.el --- Time scoping sparse trees within org -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3"))
;; Version: 0.1

;;; Commentary:

;; Navigating through an org file to see what needs to be done
;; this week and what was completed last month can be tricky.
;; This tool provides a time window to analyse your org file.

;;; Code:

;; Edit -- progn is really not necessary here, but I have no
;;         idea how to bound multiples
(require 'calendar)


;; TODO:
;;  * Cycleable user defined modes
;;  * Default time ranges to +-123wm, although how to describe that?
;;  * Second mode for precisely controlled dates.
;;  * reset org-treescope → newlib

(define-minor-mode newlib-mode8
  "Test"
  :init-value nil
  :lighter " scope"
  :keymap
  '(([left] . newlib-day-shiftrange-backwards)
    ([right] . newlib-day-shiftrange-forwards)
    ([up] . newlib-day-shiftrange-backwards-week)
    ([down] . newlib-day-shiftrange-forwards-week)
    ([C-left] . newlib-day-lowerbound-backwards)
    ([C-right] . newlib-day-lowerbound-forwards)
    ([M-left] . newlib-day-upperbound-backwards)
    ([M-right] . newlib-day-upperbound-forwards)
    ([C-M-left] . newlib-day-frommidpoint-leftwards)
    ([C-M-right] . newlib-day-frommidpoint-rightwards)
    ([C-up] . newlib-cycle-todostates-forwards)
    ([C-down] . newlib-cycle-todostates-backwards)
    ([M-up] . newlib-cycle-prioritystates-forwards)
    ([M-down] . newlib-cycle-prioritystates-backwards)
    ((kbd "r") . newlib-initialise-reset)
    ((kbd "t") . newlib-cycletimemode)))

;; -- Faces --
(defface newlib-marker-range
  '((((class color) (background light))
     :background "darkblue")
    (((class color) (background dark))
     :background "darkblue")
    (t :inverse-video t))
  "Face for showing the range markers."
  :group 'treescope-faces)

(defface newlib-marker-midday
  '((((class color) (background light))
     :background "green")
    (((class color) (background dark))
     :background "green")
    (t :inverse-video t))
  "Face for showing the middle marker."
  :group 'treescope-faces)

(defcustom newlib-range-marker 'newlib-marker-range
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'treescope)

(defcustom newlib-midday-marker 'newlib-marker-midday
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'treescope)



;; -- variables
(defvar newlib--day--leftflank nil)
(defvar newlib--day--rightflank nil)
(defvar newlib--day--frommidpoint-select nil "Possible values are `<=` and `>=`.")

;; -- Init --
(defun newlib--getmidpoint ()
  "Grabs the date under cursor (if calendar active), or returns the current date."
  (condition-case err
      (calendar-cursor-to-date nil nil)
    (error
     (ignore err)
     (calendar-current-date))))

(defsubst newlib--getmidpoint-abs ()
  "inline substitution to retrieve the current mid point in epochs."
  (calendar-absolute-from-gregorian (newlib--getmidpoint)))

(defun newlib-initialise-reset ()
  "Reset all variables and center around current date."
  (interactive)
  (setq newlib--day--leftflank nil
        newlib--day--rightflank nil
        newlib--day--frommidpoint-select nil)
  (newlib--sensible-values)
  (newlib--update-all))

(defun newlib--sensible-values ()
  "Check that all time flankers are initialised and at sensible defaults."
  ;; We deal with absolute dates, not gregorian.
  (let ((mid (newlib--getmidpoint-abs)))
    (unless newlib--day--leftflank (setq newlib--day--leftflank (- mid 3)))
    (unless newlib--day--rightflank (setq newlib--day--rightflank (+ mid 3))))
  ;; -- check sensible values --
  (if (> newlib--day--leftflank newlib--day--rightflank)
      (setq newlib--day--rightflank (+ newlib--day--leftflank 1)))
  (if (< newlib--day--rightflank newlib--day--leftflank)
      (setq newlib--day--leftflank (- newlib--day--rightflank 1))))
  ;; TODO: Add clauses for what the midpoint is doing


;; -- Date Macros
(defmacro newlib--defaults-and-updates (&rest innercode)
  "Set default NDAYS to 1 and silent to true, run INNERCODE, and then update-now."
  `(let ((ndays (if ndays ndays 1)))
     ,@innercode
     (unless silent
       (newlib--sensible-values)
       (newlib--update-all))))
(defmacro newlib--shift-ranges (direction lowerb upperb)
  "Call the LOWERB and UPPERB (low/up bounds) in DIRECTION.
Reset the `newlib--day--frommidpoint-select` to nil."
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
  ;; FIXME: work out why the midpoint jumps to end.
  (newlib-day-shiftrange-forwards 7 silent))

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
  (newlib--defaults-and-updates (setq newlib--day--frommidpoint-select "<=")))

(defun newlib-day-frommidpoint-rightwards (&optional silent)
  "Ignore left and right flanks, and select all dates after midpoint.  Don't update if SILENT."
  (interactive)
  (newlib--defaults-and-updates (setq newlib--day--frommidpoint-select ">=")))

;; -- Update method --
(defvar newlib--todogroups-state nil  "Current state of TODO custom group.")
(defvar newlib--prioritygroups-state nil  "Current state of GROUP custom group.")
(defvar newlib--timemode "TIMESTAMP"
  "Current mode to select on time. Valid values are TIMESTAMP, SCHEDULED, DEADLINE, and nil,
where nil means don't select for time at all.")

(defun newlib-cycletimemode ()
  "Cycle through the time mode selectors."
  (let* ((validmodes '(nil "TIMESTAMP" "SCHEDULED" "DEADLINE"))
         (currindex (cl-position newlib--timemode validmodes :test 'equal))
         (nextindex (mod (1+ currindex) 4))
         (nextmode (nth nextindex validmodes)))
    (setq newlib--timemode nextmode)))

(defun newlib--update-datestring ()
  "Update the date string based on current state."
  ;; For some reason newlib--shift-ranges does not parse it unless I put it here
  (if (not newlib--timemode)
      ;; FIXME: The & seperator is at the beginning when this is nil
      (format "")
    (let ((format-lambda '(lambda (x) (format "%s" x))))
      (if newlib--day--frommidpoint-select
          (let* ((gregdate-mid (calendar-gregorian-from-absolute newlib--day--midpoint))
                 (strdate-mid (mapconcat format-lambda (reverse gregdate-mid) "-")))
            ;; e.g. <=<2020-12-02> or >=<2019-01-31>
            (format "%s%s\"<%s>\""
                    newlib--timemode
                    newlib--day--frommidpoint-select
                    strdate-mid))
        ;; Otherwise set a date range.
        (let ((gregdate-left  (calendar-gregorian-from-absolute newlib--day--leftflank))
              (gregdate-right (calendar-gregorian-from-absolute newlib--day--rightflank)))
          (let ((strdate-left (mapconcat format-lambda (reverse gregdate-left) "-"))
                (strdate-right (mapconcat format-lambda (reverse gregdate-right) "-")))
            (format "%s>=\"<%s>\"&%s<=\"<%s>\""
                    newlib--timemode
                    strdate-left
                    newlib--timemode
                    strdate-right)))))))

(defun newlib--update-all (&optional silent)
  "Update the dates, todos, priorities and show on calendar if not SILENT."
  (let ((priority-string
         (if newlib--prioritygroups-state
             (eval `(format "PRIORITY>=%s&PRIORITY<=%s"
                            ,@newlib--prioritygroups-state))))
        (todo-string
         (if newlib--todogroups-state
             (let* ((string-fmt
                     (mapconcat 'identity
                                newlib--todogroups-state "\\|")))
               (format "TODO={%s}" string-fmt))))
        (date-string (newlib--update-datestring)))
    (unless silent (newlib--update-calendar))
    (setq newlib--day--frommidpoint-select nil)
    (let* ((slist `(,date-string ,todo-string ,priority-string))
           (mlist (--filter (if it it) slist))
           (formt (mapconcat 'identity mlist "&"))) ;; TODO: Become a + for priority
      (message formt))))

;; --- Todos and Priorities ---
(defcustom newlib--todogroups
  '(nil ("DONE") ("TODO" "DOING") ("TODO" "DONE") ("WAITING"))
  "List of TODO groups to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'treescope)

(defcustom newlib--prioritygroups
  '(nil (65 68) (65 70) (70 75))
  "List of PRIORITY ranges (lowest highest) to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'treescope)

(defmacro newlib--next-state (statecurrent statelist direction)
  "Set the next state in the STATELIST from the STATECURRENT, cycling in DIRECTION."
  `(let* ((now-index (or (cl-position ,statecurrent ,statelist :test 'equal) 0))
          (nxt-index (mod (,direction now-index 1) (length ,statelist)))
          (nxt-state (nth nxt-index ,statelist)))
     (setq ,statecurrent nxt-state)
     (newlib--update-all t)))

(defun newlib-cycle-todostates-forwards ()
  "Cycle the TODO groups given by the `newlib--todogroups` variable forward."
  (interactive)
  (newlib--next-state newlib--todogroups-state newlib--todogroups +))

(defun newlib-cycle-todostates-backwards ()
  "Cycle the TODO groups given by the `newlib--todogroups` variable forward."
  (interactive)
  (newlib--next-state newlib--todogroups-state newlib--todogroups -))

(defun newlib-cycle-prioritystates-forwards ()
  "Cycle the PRIORITY groups given by the `newlib--todogroups` variable forward."
  (interactive)
  (newlib--next-state newlib--prioritygroups-state newlib--prioritygroups +))

(defun newlib-cycle-prioritystates-backwards ()
  "Cycle the PRIORITY groups given by the `newlib--todogroups` variable forward."
  (interactive)
  (newlib--next-state newlib--prioritygroups-state newlib--prioritygroups -))

;; -- Calendar Functions
(defmacro newlib--markdate (abs face)
  "Takes an ABS date and highlight it on the calendar with FACE."
  `(calendar-mark-visible-date (calendar-gregorian-from-absolute ,abs) ,face))

(defun newlib--first-of-lastmonth (&optional date)
  "Grab the first day of last month, given by DATE."
  (let* ((tod (or date (calendar-current-date)))
         (mont (calendar-extract-month tod))
         (year (calendar-extract-year tod))
         (newm (- mont 1)))
    (if (> newm 0)
        (list newm 1 year)
      (list 12 1 (- year 1)))))

(defun newlib--last-of-nextmonth (&optional date)
  "Grab the last day of next month, given by DATE."
  (let* ((tod (or date (calendar-current-date)))
         (mont (calendar-extract-month tod))
         (year (calendar-extract-year tod))
         (newm (+ mont 1)))
    (if (> newm 12)
        (list 1 31 (+ year 1))
      (list newm (calendar-last-day-of-month newm year) year))))

(defun newlib--update-calendar ()
  "Show and update the calendar to show the left, right, and middle flanks."
  ;; if calendar not open
  (unless (member "*Calendar*"
                  (--map (buffer-name (window-buffer it)) (window-list)))
    (calendar))
  (newlib-mode8 t)
  (calendar-unmark)
  (let ((mid (newlib--getmidpoint-abs))
        (sel newlib--day--frommidpoint-select)
        (lfl newlib--day--leftflank)
        (rfl newlib--day--rightflank)
        ;; This might not be necessary if calendar now follows internal cursor
        (folm (calendar-absolute-from-gregorian (newlib--first-of-lastmonth)))
        (lonm (calendar-absolute-from-gregorian (newlib--last-of-nextmonth))))
    (if sel
        ;; If a flank, redefine the flanking limits
        (cond ((string= sel ">=") (setq rfl lonm lfl mid))
              ((string= sel "<=") (setq lfl folm rfl mid))))
    ;; Now colour the defined range.
    (dolist (absdate (number-sequence lfl rfl))
      (let ((visiblep (<= folm absdate lonm))
            (middlep (eq absdate mid)))
        ;; FIXME - work out when to change the calendar when midpoint not visible
        ;;         using `calendar-forward-day'.
        (if visiblep
            (if middlep
                (newlib--markdate mid newlib-midday-marker)
              (newlib--markdate absdate newlib-range-marker)))))))


(provide 'newlib)
;;; newlib.el ends here


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
