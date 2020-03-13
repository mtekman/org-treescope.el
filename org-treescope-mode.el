;;; org-treescope-mode.el --- Minor mode for org-treescope -*- lexical-binding: t; -*-

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

(provide 'org-treescope-mode)
;;; org-treescope-mode.el ends here
