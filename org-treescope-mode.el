;;; org-treescope-mode.el --- Minor mode for org-treescope -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.3") (org "9.2.3") (org-ql "0.5-pre") (dash "2.17.0"))
;; Version: 0.5

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
(require 'cal-move)

(require 'org-treescope-query)
;; brings faces, cyclestates, calendarranges, datehelper, calendar

(defvar org-treescope-mode-map
  (let ((map (make-sparse-keymap))
        (lst org-treescope-modehelper-list))
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
                  (-map (lambda (it) (buffer-name (window-buffer it)))
                        (window-list)))
    (calendar))
  (org-treescope-mode t)
  (calendar-unmark)
  (org-treescope-query--redraw-calendar))


(defun org-treescope-mode-addpublic ()
  "Add public finish functions."
  (org-treescope-mode-refresh-calendar)
  (org-treescope-query-apply-to-buffer))

(add-hook 'org-treescope-modehelper--publicfinishhook
          'org-treescope-mode-addpublic)

(provide 'org-treescope-mode)
;;; org-treescope-mode.el ends here
