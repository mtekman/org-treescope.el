;;; org-treescope-faces.el --- Faces and other customizations for org-treescope package -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3"))
;; Version: 0.1

;;; Commentary:

;; See org-treescope.el

;;; Code:
(require 'org-treescope)

;; -- Faces --
(defface org-treescope-marker-range
  '((((class color) (background light))
     :background "darkblue")
    (((class color) (background dark))
     :background "darkblue")
    (t :inverse-video t))
  "Face for showing the range markers."
  :group 'treescope-faces)

(defface org-treescope-marker-midday
  '((((class color) (background light))
     :background "green")
    (((class color) (background dark))
     :background "green")
    (t :inverse-video t))
  "Face for showing the middle marker."
  :group 'treescope-faces)

(defcustom org-treescope-range-marker 'org-treescope-marker-range
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'treescope)

(defcustom org-treescope-midday-marker 'org-treescope-marker-midday
  "How to highlight all days covered by the ranges in the calendar."
  :type '(choice (string :tag "Single character string") face)
  :group 'treescope)

(provide 'org-treescope-faces)

;;; org-treescope-faces.el ends here
