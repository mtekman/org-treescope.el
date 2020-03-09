;;; org-treescope-todosandpriority.el --- Controls and customizations for TODO and Priority states -*- lexical-binding: t; -*-

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

(defvar newlib-todogroups-state nil  "Current state of TODO custom group.")
(defvar newlib-prioritygroups-state nil  "Current state of GROUP custom group.")

(defcustom newlib-todogroups
  '(nil ("DONE") ("TODO" "DOING") ("TODO" "DONE") ("WAITING") ("CLOSED"))
  "List of TODO groups to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'treescope)

(defcustom newlib--prioritygroups
  '(nil (65 68) (65 70) (70 75))
  "List of PRIORITY ranges (lowest highest) to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'treescope)

(defvar newlib--timemode "TIMESTAMP"
  "Current mode to select on time. Valid values are TIMESTAMP, SCHEDULED, DEADLINE, and nil,
where nil means don't select for time at all.")

(defmacro newlib--next-state (statecurrent statelist direction)
  "Set the next state in the STATELIST from the STATECURRENT, cycling in DIRECTION."
  `(let* ((now-index (or (cl-position ,statecurrent ,statelist :test 'equal) 0))
          (nxt-index (mod (,direction now-index 1) (length ,statelist)))
          (nxt-state (nth nxt-index ,statelist)))
     (setq ,statecurrent nxt-state)
     (newlib--constructformat t)))

;; Todo
(defun newlib-cycle-todostates-forwards ()
  "Cycle the TODO groups given by the `newlib-todogroups` variable forward."
  (interactive)
  (newlib--next-state newlib-todogroups-state newlib-todogroups +))

(defun newlib-cycle-todostates-backwards ()
  "Cycle the TODO groups given by the `newlib-todogroups` variable forward."
  (interactive)
  (newlib--next-state newlib-todogroups-state newlib-todogroups -))

;; Priority 
(defun newlib-cycle-prioritystates-forwards ()
  "Cycle the PRIORITY groups given by the `newlib-todogroups` variable forward."
  (interactive)
  (newlib--next-state newlib-prioritygroups-state newlib--prioritygroups +))

(defun newlib-cycle-prioritystates-backwards ()
  "Cycle the PRIORITY groups given by the `newlib-todogroups` variable forward."
  (interactive)
  (newlib--next-state newlib-prioritygroups-state newlib--prioritygroups -))

;; Time
(defun newlib-cycletimemode (&optional silent)
  "Cycle through the time mode selectors."
  (interactive)
  (let* ((validmodes '(nil "TIMESTAMP" "SCHEDULED" "DEADLINE"))
         (currindex (cl-position newlib--timemode validmodes :test 'equal))
         (nextindex (mod (1+ currindex) 4))
         (nextmode (nth nextindex validmodes)))
    (setq newlib--timemode nextmode))
  (unless silent (newlib--constructformat)))


(provide 'newlib-todosandpriority)

;;; org-treescope-faces.el ends here
