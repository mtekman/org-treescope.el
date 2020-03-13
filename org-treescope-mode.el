;;; org-treescope-mode.el --- Minor mode for org-treescope -*- lexical-binding: t; -*-

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

;; Navigating through an org file to see what needs to be done
;; this week and what was completed last month can be tricky.
;; This tool provides a time window to analyse your org file.

;;; Code:
(require 'org-treescope-cyclestates) ;; brings nil
(require 'org-treescope-calendarranges) ;; brings datehelper, calendar, and faces
(require 'org-treescope-query) ;; brings faces, cyclestates, calendarranges

(defvar org-treescope-mode-map
  (let ((map (make-sparse-keymap))
        (lst '(("<left>" . org-treescope-date-shiftrange-backwards)
               ("<right>" . org-treescope-date-shiftrange-forwards)
               ("<up>" . org-treescope-date-shiftrange-backwards-week)
               ("<down>" . org-treescope-date-shiftrange-forwards-week)
               ("C-<left>" . org-treescope-date-lowerbound-backwards)
               ("C-<right>" . org-treescope-date-lowerbound-forwards)
               ("M-<left>" . org-treescope-date-upperbound-backwards)
               ("M-<right>" . org-treescope-date-upperbound-forwards)
               ("C-M-<left>" . org-treescope-date-frommidpoint-leftwards)
               ("C-M-<right>" . org-treescope-date-frommidpoint-rightwards)
               ("C-M-<down>" . org-treescope-date-frommidpoint-stop)
               ("C-<up>" . org-treescope-cycle-todo-forwards)
               ("C-<down>" . org-treescope-cycle-todo-backwards)
               ("M-<up>" . org-treescope-cycle-priority-forwards)
               ("M-<down>" . org-treescope-cycle-priority-backwards)
               ("t" . org-treescope-cycle-time-forwards))))
    (set-keymap-parent map calendar-mode-map)
    (dolist (keypair lst map)
      (define-key map (kbd (car keypair)) (cdr keypair))))
  "Keymap for function `org-treescope-mode'.")

(define-minor-mode org-treescope-mode
  "Minor Mode to control date ranges, todo and priority states."
  nil
  " scope"
  org-treescope-mode-map)

(defun org-treescope-mode-refresh-calendar ()
  "Enable the calendar and update the flanks."
  (unless (member "*Calendar*"
                  (-map (lambda (it) (buffer-name (window-buffer it))) (window-list)))
    (calendar))
  (org-treescope-mode t)
  (calendar-unmark)
  ;; perform drawing operations
  (org-treescope-query--redraw-calendar))

(defmacro org-treescope-mode-writepublicfunctions (origpref newpref type direc)
  "Write out public functions replicating those from other classes as grouped by TYPE and DIREC, changing ORIGPREF with NEWPREF."
  (let ((origfun (intern (format "org-treescope-%s--%s-%s" origpref type direc)))
        (funname (intern (format "org-treescope-%s-%s-%s" newpref type direc))))
    `;;;###autoload
    (defun ,funname ()
      (interactive)
      (,origfun)
      (org-treescope-query-apply-to-buffer)
      (org-treescope-mode-refresh-calendar))))

(org-treescope-mode-writepublicfunctions cyclestates cycle todo forwards)
(org-treescope-mode-writepublicfunctions cyclestates cycle todo backwards)
(org-treescope-mode-writepublicfunctions cyclestates cycle priority forwards)
(org-treescope-mode-writepublicfunctions cyclestates cycle priority backwards)
(org-treescope-mode-writepublicfunctions cyclestates cycle time forwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date lowerbound forwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date lowerbound backwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date upperbound forwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date upperbound backwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date frommidpoint leftwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date frommidpoint rightwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date frommidpoint stop)
(org-treescope-mode-writepublicfunctions calendarranges-day date shiftrange forwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date shiftrange backwards)
(org-treescope-mode-writepublicfunctions calendarranges-day date shiftrange forwards-week)
(org-treescope-mode-writepublicfunctions calendarranges-day date shiftrange backwards-week)

(provide 'org-treescope-mode)
;;; org-treescope-mode.el ends here
