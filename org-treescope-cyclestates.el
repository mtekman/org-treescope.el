;;; org-treescope-cyclestates.el --- Cycling  todo, priority, and time states -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3") (org-ql "0.5-pre") (dash "2.17.0"))
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
(require 'org-treescope-query)

(defcustom org-treescope-cyclestates-todo
  '(nil ("DONE") ("TODO" "DOING") ("TODO" "DONE") ("WAITING"))
  "List of TODO groups to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'org-treescope)

(defcustom org-treescope-cyclestates-priority
  '(nil ("A") ("A" "C") ("D"))
  "List of PRIORITY groups to show in buffer.  A value of nil shows all."
  :type 'list
  :group 'org-treescope)

(defcustom org-treescope-cyclestates-time
  '(nil ts ts-a ts-i scheduled deadline closed)
  "List of time range types.  A value of nil is unbounded to all time."
  :type 'list
  :group 'org-treescope)

(defvar org-treescope-cyclestates--todo-s nil  "Current state of TODO custom group.")
(defvar org-treescope-cyclestates--priority-s nil  "Current state of GROUP custom group.")
(defvar org-treescope-cyclestates--time-s 'ts  "Current mode to select on time.
Valid values are `ts', `scheduled', `deadline', (as per `org-ql') and nil,
where nil means don't select for time at all.")

(defmacro org-treescope-cyclestates--next (statecurrent statelist direction)
  "Set the next state in the STATELIST from the STATECURRENT, cycling in DIRECTION."
  `(let* ((now-index (or (cl-position ,statecurrent ,statelist :test 'equal) 0))
          (nxt-index (mod (,direction now-index 1) (length ,statelist)))
          (nxt-state (nth nxt-index ,statelist)))
     (setq ,statecurrent nxt-state)
     (org-treescope-query-apply-to-buffer)))

;; -- Todos --
;;;###autoload
(defun org-treescope-cyclestates-todo-forwards ()
  "Cycle the TODO groups given by the `org-treescope-cyclestates-todo' variable forward."
  (interactive)
  (org-treescope-cyclestates--next org-treescope-cyclestates--todo-s org-treescope-cyclestates-todo +))

;;;###autoload
(defun org-treescope-cyclestates-todo-backwards ()
  "Cycle the TODO groups given by the `org-treescope-cyclestates-todo' variable forward."
  (interactive)
  (org-treescope-cyclestates--next org-treescope-cyclestates--todo-s org-treescope-cyclestates-todo -))

;; -- Priority --
;;;###autoload
(defun org-treescope-cyclestates-priority-forwards ()
  "Cycle the PRIORITY groups given by the `org-treescope-cyclestates-priority' variable forward."
  (interactive)
  (org-treescope-cyclestates--next org-treescope-cyclestates--priority-s org-treescope-cyclestates-priority +))

;;;###autoload
(defun org-treescope-cyclestates-priority-backwards ()
  "Cycle the PRIORITY groups given by the `org-treescope-cyclestates-priority' variable forward."
  (interactive)
  (org-treescope-cyclestates--next org-treescope-cyclestates--priority-s org-treescope-cyclestates-priority -))

;; -- Times --
;;;###autoload
(defun org-treescope-cyclestates-time-forwards (&optional silent)
  "Cycle through the time mode selectors, and update the calendar if not SILENT."
  (interactive)
  (let* ((validmodes org-treescope-cyclestates-time)
         (currindex (cl-position org-treescope-cyclestates--time-s validmodes :test 'equal))
         (nextindex (mod (1+ currindex) (length validmodes)))
         (nextmode (nth nextindex validmodes)))
    (setq org-treescope-cyclestates--time-s nextmode))
  (unless silent (org-treescope-query-apply-to-buffer)))

(provide 'org-treescope-cyclestates)
;;; org-treescope-cyclestates.el ends here
