;;; org-treescope-cyclestates.el --- Cycling the different todo, priority, and time states of org-treescope -*- lexical-binding: t; -*-

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

;; see org-treescope.el

;;; Code:
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

(defvar org-treescope--state-todogroups nil  "Current state of TODO custom group.")
(defvar org-treescope--state-prioritygroups nil  "Current state of GROUP custom group.")
(defvar org-treescope--state-timemode 'ts  "Current mode to select on time.
Valid values are `ts', `scheduled', `deadline', (as per `org-ql') and nil,
where nil means don't select for time at all.")

(defmacro org-treescope--next-state (statecurrent statelist direction)
  "Set the next state in the STATELIST from the STATECURRENT, cycling in DIRECTION."
  `(let* ((now-index (or (cl-position ,statecurrent ,statelist :test 'equal) 0))
          (nxt-index (mod (,direction now-index 1) (length ,statelist)))
          (nxt-state (nth nxt-index ,statelist)))
     (setq ,statecurrent nxt-state)
     (org-treescope-apply-to-buffer)))

;; -- Todos --
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

;; -- Priority --
;;;###autoload
(defun org-treescope-cycle-prioritystates-forwards ()
  "Cycle the PRIORITY groups given by the `org-treescope-prioritygroups' variable forward."
  (interactive)
  (org-treescope--next-state org-treescope--state-prioritygroups org-treescope-prioritygroups +))

;;;###autoload
(defun org-treescope-cycle-prioritystates-backwards ()
  "Cycle the PRIORITY groups given by the `org-treescope-prioritygroups' variable forward."
  (interactive)
  (org-treescope--next-state org-treescope--state-prioritygroups org-treescope-prioritygroups -))

;; -- Times --
;;;###autoload
(defun org-treescope-cycle-timestates-forwards (&optional silent)
  "Cycle through the time mode selectors, and update the calendar if not SILENT."
  (interactive)
  (let* ((validmodes org-treescope-timegroups)
         (currindex (cl-position org-treescope--state-timemode validmodes :test 'equal))
         (nextindex (mod (1+ currindex) (length validmodes)))
         (nextmode (nth nextindex validmodes)))
    (setq org-treescope--state-timemode nextmode))
  (unless silent (org-treescope-apply-to-buffer)))

(provide 'org-treescope-cyclestates)
;;; org-treescope-cyclestates.el ends here
