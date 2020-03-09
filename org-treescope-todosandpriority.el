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
;;(require 'org-treescope)

(defvar org-treescope-todogroups-state nil  "Current state of TODO custom group.")
(defvar org-treescope-prioritygroups-state nil  "Current state of GROUP custom group.")

(defcustom org-treescope-todogroups
  '(nil ("DONE") ("TODO" "DOING") ("TODO" "DONE") ("WAITING") ("CLOSED"))
  "List of TODO groups to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'treescope)

(defcustom org-treescope--prioritygroups
  '(nil (65 68) (65 70) (70 75))
  "List of PRIORITY ranges (lowest highest) to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'treescope)

(defvar org-treescope--timemode "TIMESTAMP"
  "Current mode to select on time. Valid values are TIMESTAMP, SCHEDULED, DEADLINE, and nil,
where nil means don't select for time at all.")

(defmacro org-treescope--next-state (statecurrent statelist direction)
  "Set the next state in the STATELIST from the STATECURRENT, cycling in DIRECTION."
  `(let* ((now-index (or (cl-position ,statecurrent ,statelist :test 'equal) 0))
          (nxt-index (mod (,direction now-index 1) (length ,statelist)))
          (nxt-state (nth nxt-index ,statelist)))
     (setq ,statecurrent nxt-state)
     (org-treescope--constructformat t)))

;; Todo
(defun org-treescope-cycle-todostates-forwards ()
  "Cycle the TODO groups given by the `org-treescope-todogroups` variable forward."
  (interactive)
  (org-treescope--next-state org-treescope-todogroups-state org-treescope-todogroups +))

(defun org-treescope-cycle-todostates-backwards ()
  "Cycle the TODO groups given by the `org-treescope-todogroups` variable forward."
  (interactive)
  (org-treescope--next-state org-treescope-todogroups-state org-treescope-todogroups -))

;; Priority 
(defun org-treescope-cycle-prioritystates-forwards ()
  "Cycle the PRIORITY groups given by the `org-treescope-todogroups` variable forward."
  (interactive)
  (org-treescope--next-state org-treescope-prioritygroups-state org-treescope--prioritygroups +))

(defun org-treescope-cycle-prioritystates-backwards ()
  "Cycle the PRIORITY groups given by the `org-treescope-todogroups` variable forward."
  (interactive)
  (org-treescope--next-state org-treescope-prioritygroups-state org-treescope--prioritygroups -))

;; Time
(defun org-treescope-cycletimemode (&optional silent)
  "Cycle through the time mode selectors."
  (interactive)
  (let* ((validmodes '(nil "TIMESTAMP" "SCHEDULED" "DEADLINE"))
         (currindex (cl-position org-treescope--timemode validmodes :test 'equal))
         (nextindex (mod (1+ currindex) 4))
         (nextmode (nth nextindex validmodes)))
    (setq org-treescope--timemode nextmode))
  (unless silent (org-treescope--constructformat)))


(provide 'org-treescope-todosandpriority)

;;; org-treescope-faces.el ends here
