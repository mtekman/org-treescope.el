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

(require 'org-treescope-controls)
(require 'org-treescope-datehelpers)
(require 'org-treescope-faces)
(require 'org-treescope-todosandpriority)


;; TODO:
;;  * Cycleable user defined modes

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
    ((kbd "r") . org-treescope-start)
    ((kbd "t") . org-treescope-cycletimemode)))


(defun org-treescope-apply-to-buffer (&optional format)
  "Apply the FORMAT string on the org buffer as an argument to `org-match-sparse-tree'."
  (interactive)
  (let ((formt (if format format org-treescope--formatstring)))
    (with-current-buffer org-treescope-userbuffer
      (org-match-sparse-tree nil formt))))


;; -- Update method --
(defvar org-treescope--formatstring nil
  "The format string argument to pass to `org-match-sparse-tree' and applies to the `org-treescope-buffer'")

;;(setq org-treescope-userbuffer "projects.org")
(defcustom org-treescope-userbuffer "projects.org"
  "Apply format string to a specific user-defined buffer. Cannot be nil otherwise attempts to apply to calendar buffer.")

(defun org-treescope--constructformat (&optional silent)
  "Generates the dates, todos, priority strings, and updates the calendar SILENT."
  (let ((priority-string
         (if org-treescope--prioritygroups-state
             (eval `(format "PRIORITY>=%s&PRIORITY<=%s"
                            ,@org-treescope--prioritygroups-state))))
        (todo-string
         (if org-treescope-todogroups-state
             (let* ((string-fmt
                     (mapconcat 'identity
                                org-treescope-todogroups-state "\\|")))
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
        (message "%s%s" (if org-treescope--autoupdate-p "[Auto] " "") formt)
        (setq org-treescope--formatstring formt)
        (if org-treescope--autoupdate-p
            ;; pass format as optional param for speed
            (org-treescope--apply-to-buffer formt))))))


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
                  (org-treescope--markdate mid org-treescope-midday-marker)
                (org-treescope--markdate absdate org-treescope-range-marker))))))))


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


(defun org-treescope-start ()
  "Reset all variables and center around current date."
  (interactive)
  (setq org-treescope--day--leftflank nil
        org-treescope--day--rightflank nil
        org-treescope--day--frommidpoint-select nil)
  (org-treescope--sensible-values)
  (org-treescope--constructformat))


(provide 'org-treescope)
;;; org-treescope.el ends here
