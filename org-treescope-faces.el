;;; org-treescope-faces.el --- Face customisation for org-treescope -*- lexical-binding: t; -*-

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
(defface org-treescope-faces--markerinternal-range
  '((((class color) (background light))
     :background "darkblue")
    (((class color) (background dark))
     :background "darkblue")
    (t :inverse-video t))
  "Face for showing the range markers."
  :group 'treescope-faces)

(defface org-treescope-faces--markerinternal-midday
  '((((class color) (background light))
     :background "green")
    (((class color) (background dark))
     :background "green")
    (t :inverse-video t))
  "Face for showing the middle marker."
  :group 'treescope-faces)

(defcustom org-treescope-faces-range 'org-treescope-faces--markerinternal-range
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'org-treescope)
(defcustom org-treescope-faces-midday 'org-treescope-faces--markerinternal-midday
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'org-treescope)

(provide 'org-treescope-faces)
;;; org-treescope-faces.el ends here
