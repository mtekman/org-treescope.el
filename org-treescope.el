;;; org-treescope.el --- Time scoping sparse trees within org -*- lexical-binding: t; -*-

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

;; Navigating through an org file to see what needs to be done
;; this week and what was completed last month can be tricky.
;; This tool provides a time window to analyse your org file.

;;; Code:

(require 'calendar)
;; NOTE TO REVIEWER: calendar.el is not a lexically-bound library, does
;;                   this mean I shouldn't add lexical-binding to mine?
(require 'org-ql)

;;(defvar
(setq org-treescope-mode-map
      (let ((map (make-sparse-keymap))
            (lst '(("<left>" . org-treescope-day-shiftrange-backwards)
                   ("<right>" . org-treescope-day-shiftrange-forwards)
                   ("<up>" . org-treescope-day-shiftrange-backwards-week)
                   ("<down>" . org-treescope-day-shiftrange-forwards-week)
                   ("C-<left>" . org-treescope-day-lowerbound-backwards)
                   ("C-<right>" . org-treescope-day-lowerbound-forwards)
                   ("M-<left>" . org-treescope-day-upperbound-backwards)
                   ("M-<right>" . org-treescope-day-upperbound-forwards)
                   ("C-M-<left>" . org-treescope-day-frommidpoint-leftwards)
                   ("C-M-<right>" . org-treescope-day-frommidpoint-rightwards)
                   ("C-M-<down>" . org-treescope-day-frommidpoint-stop)
                   ("C-<up>" . org-treescope-cycle-todostates-forwards)
                   ("C-<down>" . org-treescope-cycle-todostates-backwards)
                   ("M-<up>" . org-treescope-cycle-prioritystates-forwards)
                   ("M-<down>" . org-treescope-cycle-prioritystates-backwards)
                   ("t" . org-treescope-cycle-timestates-forwards))))
        (set-keymap-parent map calendar-mode-map)
        (dolist (keypair lst map)
          (define-key map (kbd (car keypair)) (cdr keypair)))))
;;"Keymap for function `org-treescope-mode'.")

(define-minor-mode org-treescope-mode8
  "Minor Mode to control date ranges, todo and priority states."
  nil
  " scope"
  org-treescope-mode-map)

(defgroup org-treescope nil
  "org-treescope customisable variables."
  :group 'productivity)

(setq org-treescope-userbuffer "~/repos/org-projects/gtd/projects.org")
(setq org-treescope-prioritygroups '(nil ("A") ("A" "C") ("D")))
(setq org-treescope-todogroups '(nil ("DONE") ("TODO" "DOING") ("TODO" "DONE") ("WAITING")))
(setq org-treescope-timegroups '(nil ts scheduled deadline closed))
;; users can set ts-i ts-i clocked planning

(defcustom org-treescope-userbuffer nil
  "Apply match function to a specific user-defined `org-mode' file.  Cannot be nil otherwise attempts to apply to calendar buffer."
  :type 'string
  :group 'org-treescope)

(defcustom org-treescope-todogroups
  '(nil ("DONE") ("TODO" "DOING") ("TODO" "DONE") ("WAITING"))
  "List of TODO groups to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'org-treescope)

(defcustom org-treescope-prioritygroups
  '(nil ("A") ("A" "C") ("D"))
  "List of PRIORITY groups to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'org-treescope)

(defcustom org-treescope-timegroups
  '(nil ts ts-a ts-i scheduled deadline closed)
  "List of time range types.  A value of nil is unbounded to all time."
  :type 'list
  :group 'org-treescope)

;; -- Faces --
(defface org-treescope--markerinternal-range
  '((((class color) (background light))
     :background "darkblue")
    (((class color) (background dark))
     :background "darkblue")
    (t :inverse-video t))
  "Face for showing the range markers."
  :group 'treescope-faces)

(defface org-treescope--markerinternal-midday
  '((((class color) (background light))
     :background "green")
    (((class color) (background dark))
     :background "green")
    (t :inverse-video t))
  "Face for showing the middle marker."
  :group 'treescope-faces)

(defcustom org-treescope-marker-range 'org-treescope--markerinternal-range
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'org-treescope)
(defcustom org-treescope-marker-midday 'org-treescope--markerinternal-midday
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'org-treescope)

(defvar org-treescope--day--leftflank nil)
(defvar org-treescope--day--rightflank nil)
(defvar org-treescope--day--frommidpoint-select nil "Possible values are `:to' and `:from'.")

(defvar org-treescope--timemode 'ts
  "Current mode to select on time.
Valid values are `ts', `scheduled', `deadline', (as per `org-ql') and nil,
where nil means don't select for time at all.")

(defvar org-treescope--state-todogroups nil  "Current state of TODO custom group.")
(defvar org-treescope--state-prioritygroups nil  "Current state of GROUP custom group.")
(defvar org-treescope--formatstring nil
  "The format string argument to pass to `org-ql-sparse-tree' and applies to the `org-treescope-userbuffer'.")


;;;;;;;;;;;;;;; org-treescope-controls.el starts here ;;;;;;;;;;;;;;;
(defsubst org-treescope--getmidpoint () ;; getmidpoint-abs
  "Grabs the date under cursor (if calendar active), or return the current date."
  (condition-case err
      (calendar-cursor-to-date nil nil)
    (error
     (ignore err)
     (calendar-current-date))))

(defsubst org-treescope--getmidpoint-abs () ;; called by sensible-values and redraw-calendar
  "Inline substitution to retrieve the current mid point in epochs."
  (calendar-absolute-from-gregorian (org-treescope--getmidpoint)))

;; -- Macros
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


;; -- Date Methods
;;;###autoload
(defun org-treescope-day-shiftrange-backwards (&optional ndays silent)
  "Shift entire range back by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-ranges - org-treescope-day-lowerbound-backwards org-treescope-day-upperbound-backwards))

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

;;;###autoload
(defun org-treescope-day-shiftrange-forwards (&optional ndays silent)
  "Shift entire range forwards by NDAYS and update midpoint.  Don't update if SILENT."
  (interactive)
  (org-treescope--shift-ranges + org-treescope-day-lowerbound-forwards org-treescope-day-upperbound-forwards))

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
;;;;;;;;;;;;;;; org-treescope-controls.el stops about here ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; org-treescope--datehelpers starts here ;;;;;;;;;;;;;;;
(defmacro org-treescope--markdate (abs face) ;; redraw-calendar
  "Takes an ABS date and highlight it on the calendar with FACE."
  `(calendar-mark-visible-date (calendar-gregorian-from-absolute ,abs) ,face))

;; -- from calendar.el, redefine here to remove lint and compiler warnings
(defvar displayed-month)
(defvar displayed-year)

(defun org-treescope--first-of-lastmonth ()
  "Grab the first day of last month of current calendar window.  Used by `org-treescope--redraw-calendar'."
  (let* ((mont displayed-month)
         (year displayed-year)
         (newm (- mont 1)))
    (if (> newm 0)
        (list newm 1 year)
      (list 12 1 (- year 1)))))

(defun org-treescope--last-of-nextmonth ()
  "Grab the last day of next month of current calendar window.  Used by `org-treescope--redraw-calendar'."
  (let* ((mont displayed-month)
         (year displayed-year)
         (newm (+ mont 1)))
    (if (> newm 12)
        (list 1 31 (+ year 1))
      (list newm (calendar-last-day-of-month newm year) year))))

(defsubst org-treescope--datetostring (gregdate)
  "Convert GREGDATE to an org compatible date.  Used by `org-treescope--redraw-calendar'."
  (let ((year (nth 2 gregdate))
        (month (nth 0 gregdate))
        (day (nth 1 gregdate)))
    (format "%04d-%02d-%02d" year month day)))

(defun org-treescope--generate-datestring () ;; construct-format
  "Generate the date string based on current state."
  (when org-treescope--timemode
    (if org-treescope--day--frommidpoint-select
        (let* ((gregdate-mid (calendar-cursor-to-date))
               (strdate-mid (org-treescope--datetostring gregdate-mid)))
          ;; e.g. :to<2020-12-02> or :from<2019-01-31>
          `(,org-treescope--timemode ,org-treescope--day--frommidpoint-select ,strdate-mid))
      ;; Otherwise set a date range.
      (let ((gregdate-left  (calendar-gregorian-from-absolute org-treescope--day--leftflank))
            (gregdate-right (calendar-gregorian-from-absolute org-treescope--day--rightflank)))
        (let ((strdate-left (org-treescope--datetostring gregdate-left))
              (strdate-right (org-treescope--datetostring gregdate-right)))
          `(,org-treescope--timemode :from ,strdate-left :to ,strdate-right))))))
;;;;;;;;;;;;;;; org-treescope-datehelpers.el stops about here ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; org-treescope-todosandpriority starts about here ;;;;;;;;;;;;;;;
(defmacro org-treescope--next-state (statecurrent statelist direction)
  "Set the next state in the STATELIST from the STATECURRENT, cycling in DIRECTION."
  `(let* ((now-index (or (cl-position ,statecurrent ,statelist :test 'equal) 0))
          (nxt-index (mod (,direction now-index 1) (length ,statelist)))
          (nxt-state (nth nxt-index ,statelist)))
     (setq ,statecurrent nxt-state)
     (org-treescope-apply-to-buffer)))

;; Todo
;;;###autoload
(defun org-treescope-cycle-todostates-forwards ()
  "Cycle the TODO groups given by the `org-treescope-todogroups' variable forward."
  (interactive)
  (org-treescope--next-state org-treescope--state-todogroups org-treescope-todogroups +))

;;;###autoload
(defun org-treescope-cycle-todostates-backwards ()
  "Cycle the TODO groups given by the `org-treescope-todogroups' variable forward."
  (interactive)
  (org-treescope--next-state org-treescope--state-todogroups org-treescope-todogroups -))

;; Priority
;;;###autoload
(defun org-treescope-cycle-prioritystates-forwards ()
  "Cycle the PRIORITY groups given by the `org-treescope-todogroups' variable forward."
  (interactive)
  (org-treescope--next-state org-treescope--state-prioritygroups org-treescope-prioritygroups +))

;;;###autoload
(defun org-treescope-cycle-prioritystates-backwards ()
  "Cycle the PRIORITY groups given by the `org-treescope-todogroups' variable forward."
  (interactive)
  (org-treescope--next-state org-treescope--state-prioritygroups org-treescope-prioritygroups -))

;; Time
;;;###autoload
(defun org-treescope-cycle-timestates-forwards (&optional silent)
  "Cycle through the time mode selectors, and update the calendar if not SILENT."
  (interactive)
  (let* ((validmodes org-treescope-timegroups)
         (currindex (cl-position org-treescope--timemode validmodes :test 'equal))
         (nextindex (mod (1+ currindex) (length validmodes)))
         (nextmode (nth nextindex validmodes)))
    (setq org-treescope--timemode nextmode))
  (unless silent (org-treescope-apply-to-buffer)))

;;;;;;;;;;;;;;; org-treescope-todosandpriority.el ends about here ;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;; org-treescope.el starts about here ;;;;;;;;;;;;;;;
(defun org-treescope--constructformat ()
  "Generate the dates, todos, priority strings."
  (let ((priority-symbol
         (if org-treescope--state-prioritygroups
             `(priority ,@org-treescope--state-prioritygroups)))
        (todo-symbol
         (if org-treescope--state-todogroups
             `(todo ,@org-treescope--state-todogroups)))
        (date-symbol (org-treescope--generate-datestring)))
    ;; -- Construct a sensible format
    (let* ((slist `(,date-symbol ,todo-symbol ,priority-symbol))
           (mlist (--filter (if it it) slist)))
      (if mlist
          (if (eq 1 (length mlist))
              (car mlist)
            `(and ,@mlist))))))

;;;###autoload
(defun org-treescope-apply-to-buffer (&optional format)
  "Apply the FORMAT string on the org buffer as an argument to `org-ql-sparse-tree'."
  (interactive)
  ;; TODO: a sit-for delay to show this message.
  ;;(message "Applying...")
  (org-treescope--redraw-calendar)
  (let ((formt (if format format (org-treescope--constructformat))))
    (with-current-buffer (find-file-noselect org-treescope-userbuffer)
      (when formt
        ;; FIXME: save-excursion does not work here....
        (org-ql-sparse-tree formt)
        (message (format "%s" formt))
        (goto-char 0)))))


(defun org-treescope--redraw-calendar ()
  "Show and update the calendar to show the left, right, and middle flanks."
  ;; if calendar not open
  (unless (member
           "*Calendar*"
           (--map (buffer-name (window-buffer it)) (window-list)))
    (calendar))
  (org-treescope-mode8 t)
  ;; perform drawing operations
  (calendar-unmark)
  (when org-treescope--timemode
    (let ((mid (org-treescope--getmidpoint-abs))
          (sel org-treescope--day--frommidpoint-select)
          (lfl org-treescope--day--leftflank)
          (rfl org-treescope--day--rightflank)
          ;; This might not be necessary if calendar now follows internal cursor
          (folm (calendar-absolute-from-gregorian (org-treescope--first-of-lastmonth)))
          (lonm (calendar-absolute-from-gregorian (org-treescope--last-of-nextmonth))))
      (if sel
          ;; If a flank, redefine the flanking limits
          (cond ((eq sel :from) (setq rfl lonm
                                      lfl mid))
                ((eq sel :to) (setq lfl folm
                                    rfl mid))))
      ;; Now colour the defined range.
      (dolist (absdate (number-sequence lfl rfl))
        (let ((visiblep (<= folm absdate lonm))
              (middlep (eq absdate mid)))
          (if visiblep
              (if middlep
                  (org-treescope--markdate mid org-treescope-marker-midday)
                (org-treescope--markdate absdate org-treescope-marker-range))))))))


(defun org-treescope--sensible-values () ;; org-treescope-start
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
  ;; TODO: Add clauses for what the midpoint is doing

;;;###autoload
(defun org-treescope ()
  "Reset all variables and center around current date."
  (interactive)
  (setq org-treescope--day--leftflank nil
        org-treescope--day--rightflank nil
        org-treescope--day--frommidpoint-select nil)
  (org-treescope--sensible-values)
  (org-treescope-apply-to-buffer))

(provide 'org-treescope)
;;; org-treescope.el ends here
