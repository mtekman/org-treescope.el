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
(require 'org-ql)

(require 'org-treescope-faces)
(require 'org-treescope-cyclestates)
(require 'org-treescope-calendarranges)
(require 'org-treescope-datehelper)

(defvar org-treescope-mode-map
  (let ((map (make-sparse-keymap))
        (lst '(("<left>" . org-treescope-calendarranges-day-shiftrange-backwards)
               ("<right>" . org-treescope-calendarranges-day-shiftrange-forwards)
               ("<up>" . org-treescope-calendarranges-day-shiftrange-backwards-week)
               ("<down>" . org-treescope-calendarranges-day-shiftrange-forwards-week)
               ("C-<left>" . org-treescope-calendarranges-day-lowerbound-backwards)
               ("C-<right>" . org-treescope-calendarranges-day-lowerbound-forwards)
               ("M-<left>" . org-treescope-calendarranges-day-upperbound-backwards)
               ("M-<right>" . org-treescope-calendarranges-day-upperbound-forwards)
               ("C-M-<left>" . org-treescope-calendarranges-day-frommidpoint-leftwards)
               ("C-M-<right>" . org-treescope-calendarranges-day-frommidpoint-rightwards)
               ("C-M-<down>" . org-treescope-calendarranges-day-frommidpoint-stop)
               ("C-<up>" . org-treescope-cyclestates-todo-forwards)
               ("C-<down>" . org-treescope-cyclestates-todo-backwards)
               ("M-<up>" . org-treescope-cyclestates-priority-forwards)
               ("M-<down>" . org-treescope-cyclestates-priority-backwards)
               ("t" . org-treescope-cyclestates-time-forwards))))
    (set-keymap-parent map calendar-mode-map)
    (dolist (keypair lst map)
      (define-key map (kbd (car keypair)) (cdr keypair))))
  "Keymap for function `org-treescope-mode'.")

(define-minor-mode org-treescope-mode
  "Minor Mode to control date ranges, todo and priority states."
  nil
  " scope"
  org-treescope-mode-map)

(defgroup org-treescope nil "org-treescope customisable variables."
  :group 'productivity)

(defcustom org-treescope-userbuffer nil
  "Apply match function to a specific user-defined `org-mode' file.  Cannot be nil otherwise attempts to apply to calendar buffer."
  :type 'string
  :group 'org-treescope)

(defun org-treescope--generate-datestring ()
  "Generate the date string based on current state."
  (when org-treescope-cyclestates--time-s
    (if org-treescope-calendarranges--day--frommidpoint-select
        (let* ((gregdate-mid (calendar-cursor-to-date))
               (strdate-mid (org-treescope-datehelper--datetostring gregdate-mid)))
          ;; e.g. :to<2020-12-02> or :from<2019-01-31>
          `(,org-treescope-cyclestates--time-s ,org-treescope-calendarranges--day--frommidpoint-select ,strdate-mid))
      ;; Otherwise set a date range.
      (let ((gregdate-left  (calendar-gregorian-from-absolute org-treescope-calendarranges--day--leftflank))
            (gregdate-right (calendar-gregorian-from-absolute org-treescope-calendarranges--day--rightflank)))
        (let ((strdate-left (org-treescope-datehelper--datetostring gregdate-left))
              (strdate-right (org-treescope-datehelper--datetostring gregdate-right)))
          `(,org-treescope-cyclestates--time-s :from ,strdate-left :to ,strdate-right))))))

(defun org-treescope--make-query ()
  "Generate the query from dates, todos and priority states."
  (let ((priority-symbol
         (if org-treescope-cyclestates--priority-s
             `(priority ,@org-treescope-cyclestates--priority-s)))
        (todo-symbol
         (if org-treescope-cyclestates--todo-s
             `(todo ,@org-treescope-cyclestates--todo-s)))
        (date-symbol (org-treescope--generate-datestring)))
    ;; -- Construct a sensible format
    (let* ((working-list (-non-nil `(,date-symbol ,todo-symbol ,priority-symbol))))
      (if working-list
          (if (eq 1 (length working-list))
              (car working-list)
            `(and ,@working-list))))))

;;;###autoload
(defun org-treescope-apply-to-buffer (&optional query)
  "Apply the QUERY to the org buffer as an argument to `org-ql-sparse-tree'."
  (interactive)
  (org-treescope-calendarranges--redraw-calendar)
  (let ((query (if query query (org-treescope--make-query))))
    (when query
      (with-current-buffer (find-file-noselect org-treescope-userbuffer)
        (let ((pos (point)))
          (org-ql-sparse-tree query)
          (goto-char pos)
          (message (format "%s" query)))))))

;;;###autoload
(defun org-treescope ()
  "Reset all variables and center around current date."
  (interactive)
  (setq org-treescope-calendarranges--day--leftflank nil
        org-treescope-calendarranges--day--rightflank nil
        org-treescope-calendarranges--day--frommidpoint-select nil)
  (org-treescope-calendarranges--sensible-values)
  (org-treescope-apply-to-buffer))

(provide 'org-treescope)
;;; org-treescope.el ends here
