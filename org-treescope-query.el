;;; org-treescope-query.el --- Constructing and applying org-ql queries -*- lexical-binding: t; -*-

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
(defun org-treescope-query--generate-datestring ()
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

(defun org-treescope-query--make-query ()
  "Generate the query from dates, todos and priority states."
  (let ((priority-symbol
         (if org-treescope-cyclestates--priority-s
             `(priority ,@org-treescope-cyclestates--priority-s)))
        (todo-symbol
         (if org-treescope-cyclestates--todo-s
             `(todo ,@org-treescope-cyclestates--todo-s)))
        (date-symbol (org-treescope-query--generate-datestring)))
    ;; -- Construct a sensible format
    (let* ((working-list (-non-nil `(,date-symbol ,todo-symbol ,priority-symbol))))
      (if working-list
          (if (eq 1 (length working-list))
              (car working-list)
            `(and ,@working-list))))))

;;;###autoload
(defun org-treescope-query-apply-to-buffer (&optional query)
  "Apply the QUERY to the org buffer as an argument to `org-ql-sparse-tree'."
  (interactive)
  (org-treescope-calendarranges--redraw-calendar)
  (let ((query (if query query (org-treescope-query--make-query))))
    (when query
      (with-current-buffer (find-file-noselect org-treescope-userbuffer)
        (let ((pos (point)))
          (org-ql-sparse-tree query)
          (goto-char pos)
          (message (format "%s" query)))))))

(provide 'org-treescope-query)
;;; org-treescope-query.el ends here
