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

;; TODO:
;;  * Figure out how to split these into separate files for compiling...

(define-minor-mode org-treescope-mode8
  "Test"
  :init-value nil
  :lighter " scope"
  :keymap
  '(([left] . org-treescope-day-shiftrange-backwards)
    ([right] . org-treescope-day-shiftrange-forwards)
    ([up] . org-treescope-day-shiftrange-backwards-week)
    ([down] . org-treescope-day-shiftrange-forwards-week)
    ([C-left] . org-treescope-day-lowerbound-backwards)
    ([C-right] . org-treescope-day-lowerbound-forwards)
    ([M-left] . org-treescope-day-upperbound-backwards)
    ([M-right] . org-treescope-day-upperbound-forwards)
    ([C-M-left] . org-treescope-day-frommidpoint-leftwards)
    ([C-M-right] . org-treescope-day-frommidpoint-rightwards)
    ([C-M-down] . org-treescope-day-frommidpoint-stop)
    ([C-up] . org-treescope-cycle-todostates-forwards)
    ([C-down] . org-treescope-cycle-todostates-backwards)
    ([M-up] . org-treescope-cycle-prioritystates-forwards)
    ([M-down] . org-treescope-cycle-prioritystates-backwards)
    ([return] . org-treescope-apply-to-buffer)
    ((kbd "f") . org-treescope-toggleautoupdate)
    ((kbd "t") . org-treescope-cycletimemode)))

(setq org-treescope-userbuffer "~/repos/org-projects/gtd/projects.org")
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
  '(nil (65 68) (65 70) (70 75))
  "List of PRIORITY ranges (lowest highest) to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'org-treescope)

(defcustom org-treescope-timegroups
  '(nil "TIMESTAMP" "SCHEDULED" "DEADLINE" "CLOSED")
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
(defvar org-treescope--day--frommidpoint-select nil "Possible values are `<=` and `>=`.")

(defvar org-treescope--timemode "TIMESTAMP"
  "Current mode to select on time.
Valid values are TIMESTAMP, SCHEDULED, DEADLINE, and nil,
where nil means don't select for time at all.")

(defvar org-treescope--state-todogroups nil  "Current state of TODO custom group.")
(defvar org-treescope--state-prioritygroups nil  "Current state of GROUP custom group.")
(defvar org-treescope--formatstring nil
  "The format string argument to pass to `org-match-sparse-tree' and applies to the `org-treescope-buffer'.")



;;;;;;;;;;;;;;; org-treescope-controls.el starts here ;;;;;;;;;;;;;;;
(defsubst org-treescope--getmidpoint () ;; getmidpoint-abs
  "Grabs the date under cursor (if calendar active), or return the current date."
  (condition-case err
      (calendar-cursor-to-date nil nil)
    (error
     (ignore err)
     (calendar-current-date))))

(defsubst org-treescope--getmidpoint-abs () ;; called by sensible-values and update-calendar
  "Inline substitution to retrieve the current mid point in epochs."
  (calendar-absolute-from-gregorian (org-treescope--getmidpoint)))

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
(defun org-treescope-day-frommidpoint-leftwards (&optional silent)
  "Ignore left and right flanks, and select all dates before midpoint.  Don't update if SILENT."
  (interactive)
  (let ((ndays nil))
    (org-treescope--defaults-and-updates (setq org-treescope--day--frommidpoint-select "<="))))

;;;###autoload
(defun org-treescope-day-frommidpoint-rightwards (&optional silent)
  "Ignore left and right flanks, and select all dates after midpoint.  Don't update if SILENT."
  (interactive)
  (let ((ndays nil))
    (org-treescope--defaults-and-updates (setq org-treescope--day--frommidpoint-select ">="))))

;;;###autoload
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

;;;;;;;;;;;;;;; org-treescope-controls.el stops about here ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; org-treescope--datehelpers starts here ;;;;;;;;;;;;;;;
(require 'calendar)

(defvar org-treescope--autoupdate-p t ;; used by toggleautoupdate and construct-format
  "Automatically apply the current format string on every user update.")

;;;###autoload
(defun org-treescope-toggleautoupdate ()
  "Toggle the auto-update capability for every user-action."
  (interactive)
  (setq org-treescope--autoupdate-p (not org-treescope--autoupdate-p)))

(defmacro org-treescope--markdate (abs face) ;; update-calendar
  "Takes an ABS date and highlight it on the calendar with FACE."
  `(calendar-mark-visible-date (calendar-gregorian-from-absolute ,abs) ,face))

;; -- from calendar.el, redefine here to remove lint and compiler warnings
(defvar displayed-month)
(defvar displayed-year)

(defun org-treescope--first-of-lastmonth ()
  "Grab the first day of last month of current calendar window.  Used by `org-treescope--update-calendar'."
  (let* ((mont displayed-month)
         (year displayed-year)
         (newm (- mont 1)))
    (if (> newm 0)
        (list newm 1 year)
      (list 12 1 (- year 1)))))

(defun org-treescope--last-of-nextmonth ()
  "Grab the last day of next month of current calendar window.  Used by `org-treescope--update-calendar'."
  (let* ((mont displayed-month)
         (year displayed-year)
         (newm (+ mont 1)))
    (if (> newm 12)
        (list 1 31 (+ year 1))
      (list newm (calendar-last-day-of-month newm year) year))))

(defsubst org-treescope--datetostring (gregdate)
  "Convert GREGDATE to an org compatible date.  Used by `org-treescope--update-calendar'."
  (let ((year (nth 2 gregdate))
        (month (nth 0 gregdate))
        (day (nth 1 gregdate)))
    (format "%04d-%02d-%02d" year month day)))

(defun org-treescope--update-datestring () ;; construct-format
  "Update the date string based on current state."
  (when org-treescope--timemode
    (if org-treescope--day--frommidpoint-select
        (let* ((gregdate-mid (calendar-cursor-to-date))
               (strdate-mid (org-treescope--datetostring gregdate-mid)))
          ;; e.g. <=<2020-12-02> or >=<2019-01-31>
          (format "%s%s\"<%s>\""
                  org-treescope--timemode
                  org-treescope--day--frommidpoint-select
                  strdate-mid))
      ;; Otherwise set a date range.
      (let ((gregdate-left  (calendar-gregorian-from-absolute org-treescope--day--leftflank))
            (gregdate-right (calendar-gregorian-from-absolute org-treescope--day--rightflank)))
        (let ((strdate-left (org-treescope--datetostring gregdate-left))
              (strdate-right (org-treescope--datetostring gregdate-right)))
          (format "%s>=\"<%s>\"&%s<=\"<%s>\""
                  org-treescope--timemode
                  strdate-left
                  org-treescope--timemode
                  strdate-right))))))


;;;;;;;;;;;;;;; org-treescope-datehelpers.el stops about here ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; org-treescope-todosandpriority starts about here ;;;;;;;;;;;;;;;
(defmacro org-treescope--next-state (statecurrent statelist direction)
  "Set the next state in the STATELIST from the STATECURRENT, cycling in DIRECTION."
  `(let* ((now-index (or (cl-position ,statecurrent ,statelist :test 'equal) 0))
          (nxt-index (mod (,direction now-index 1) (length ,statelist)))
          (nxt-state (nth nxt-index ,statelist)))
     (setq ,statecurrent nxt-state)
     (org-treescope--constructformat t)))

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
(defun org-treescope-cycletimemode (&optional silent)
  "Cycle through the time mode selectors, and update the calendar if not SILENT."
  (interactive)
  (let* ((validmodes org-treescope-timegroups)
         (currindex (cl-position org-treescope--timemode validmodes :test 'equal))
         (nextindex (mod (1+ currindex) (length validmodes)))
         (nextmode (nth nextindex validmodes)))
    (setq org-treescope--timemode nextmode))
  (unless silent (org-treescope--constructformat)))

;;;;;;;;;;;;;;; org-treescope-todosandpriority.el ends about here ;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;; org-treescope.el starts about here ;;;;;;;;;;;;;;;
;;;###autoload
(defun org-treescope-apply-to-buffer (&optional format)
  "Apply the FORMAT string on the org buffer as an argument to `org-match-sparse-tree'."
  (interactive)
  ;; TODO: a sit-for delay to show this message.
  ;;(message "Applying...")
  (let ((formt (if format format org-treescope--formatstring)))
    (with-current-buffer (find-file-noselect org-treescope-userbuffer)
      (org-match-sparse-tree nil formt)
      (message formt))))

;; -- Update method --
(defun org-treescope--constructformat (&optional silent)
  "Generate the dates, todos, priority strings, and don't update the calendar if SILENT."
  (let ((priority-string
         (if org-treescope--state-prioritygroups
             (eval `(format "PRIORITY>=%s&PRIORITY<=%s"
                            ,@org-treescope--state-prioritygroups))))
        (todo-string
         (if org-treescope--state-todogroups
             (let* ((string-fmt
                     (mapconcat 'identity
                                org-treescope--state-todogroups "\\|")))
               (format "TODO={%s}" string-fmt))))
        (date-string (org-treescope--update-datestring)))
    (setq org-treescope--formatstring nil)  ; reset format string
    ;;
    (unless silent (org-treescope--update-calendar))
    ;;
    (let* ((slist `(,date-string ,todo-string ,priority-string))
           (mlist (--filter (if it it) slist))
           (formt (mapconcat 'identity mlist "&"))) ;; TODO: Become a + for priority
      (when formt
        (message "%s%s" formt (if org-treescope--autoupdate-p "  [Auto]" ""))
        (setq org-treescope--formatstring formt)
        (if org-treescope--autoupdate-p
            ;; pass format as optional param for speed
            (org-treescope-apply-to-buffer formt))))))


(defun org-treescope--update-calendar ()
  "Show and update the calendar to show the left, right, and middle flanks."
  ;; if calendar not open
  (unless (member "*Calendar*"
                  (--map (buffer-name (window-buffer it)) (window-list)))
    (calendar))
  (org-treescope-mode8 t)
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
          (cond ((string= sel ">=") (setq rfl lonm lfl mid))
                ((string= sel "<=") (setq lfl folm rfl mid))))
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
  (org-treescope--constructformat))

(provide 'org-treescope)
;;;;;;;;;;;;;;; org-treescope.el starts about here ;;;;;;;;;;;;;;;

;;; org-treescope.el ends here
