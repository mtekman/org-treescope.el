;;; org-treescope-modehelper.el --- File for populating org-treescope modes  -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'org-treescope-query)

(defvar org-treescope-modehelper-list nil
  "Alist of KEY BINDING pairs, populated throughout.")

;; these two functions are added to every autoload
(org-treescope-mode-refresh-calendar)
(org-treescope-query-apply-to-buffer)

(defun org-treescope-mode-refresh-calendar ()
  "Enable the calendar and update the flanks."
  (unless (member "*Calendar*"
                  (-map (lambda (it) (buffer-name (window-buffer it))) (window-list)))
    (calendar))
  ;;(org-treescope-mode t) -- add hook here instead in the main part
  (calendar-unmark)
  (org-treescope-query--redraw-calendar))


(provide 'org-treescope-modehelper)
;;; org-treescope-modehelper.el ends here