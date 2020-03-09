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
    ([C-M-down] . newlib-day-frommidpoint-stop)
    ([C-up] . newlib-cycle-todostates-forwards)
    ([C-down] . newlib-cycle-todostates-backwards)
    ([M-up] . newlib-cycle-prioritystates-forwards)
    ([M-down] . newlib-cycle-prioritystates-backwards)
    ([return] . newlib-apply-to-buffer)
    ((kbd "f") . newlib-toggleautoupdate)
    ((kbd "r") . newlib-start)
    ((kbd "t") . newlib-cycletimemode)))




(defun newlib-start ()
  "Reset all variables and center around current date."
  (interactive)
  (setq newlib--day--leftflank nil
        newlib--day--rightflank nil
        newlib--day--frommidpoint-select nil)
  (newlib--sensible-values)
  (newlib--constructformat))

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

  (interactive)

  (interactive)


;; -- Update method --


(defsubst newlib--datetostring (gregdate)
  ;; TODO: Make sure the date is 0 padded otherwise nothing is shown
  (let ((revdate (reverse gregdate)))
    (eval `(format "%04d-%02d-%02d" ,@revdate))))

(defun newlib--update-datestring ()
  "Update the date string based on current state."
  ;; For some reason newlib--shift-ranges does not parse it unless I put it here
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


(defvar newlib--autoupdate-p t
  "Automatically apply the current format string on every user update.")

(defvar newlib--formatstring nil
  "The format string argument to pass to `org-match-sparse-tree' and applies to the `newlib-buffer'")

;(setq newlib-userbuffer "projects.org")
(defcustom newlib-userbuffer "projects.org"
  "Apply format string to a specific user-defined buffer. Cannot be nil otherwise attempts to apply to calendar buffer.")

(defun newlib-apply-to-buffer (&optional format bname)
  "Apply the FORMAT string on the org buffer BNAME as an argument to `org-match-sparse-tree'."
  (interactive)
  (let ((formt (if format format newlib--formatstring)))
    (with-current-buffer newlib-userbuffer
      (org-match-sparse-tree nil formt))))

(defun newlib-toggleautoupdate ()
  "Toggle the auto-update capability for every user-action."
  (interactive)
  (setq newlib--autoupdate-p (not newlib--autoupdate-p)))

(defun newlib--constructformat (&optional silent)
  "Generates the dates, todos, priority strings, and updates the calendar SILENT."
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
    (setq newlib--formatstring nil)  ; reset format string
    (unless silent (newlib--update-calendar))
    (let* ((slist `(,date-string ,todo-string ,priority-string))
           (mlist (--filter (if it it) slist))
           (formt (mapconcat 'identity mlist "&"))) ;; TODO: Become a + for priority
      (when formt
        (message "%s%s" (if newlib--autoupdate-p "[Auto] " "") formt)
        (setq newlib--formatstring formt)
        (if newlib--autoupdate-p
            ;; pass format as optional param for speed
            (newlib--apply-to-buffer formt))))))


(defun newlib--update-calendar ()
  "Show and update the calendar to show the left, right, and middle flanks."
  ;; if calendar not open
  (unless (member "*Calendar*"
                  (--map (buffer-name (window-buffer it)) (window-list)))
    (calendar))
  (newlib-mode8 t)
  (calendar-unmark)
  (when newlib--timemode
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
          (if visiblep
              (if middlep
                  (newlib--markdate mid newlib-midday-marker)
                (newlib--markdate absdate newlib-range-marker))))))))


(provide 'newlib)
;;; newlib.el ends here


;; Attempt to macrofy interactive functions, does not save lines


