;;; org-treescope.el --- Time scoping sparse trees within org -*- lexical-binding: t; -*-

;; Copright (C) 2019 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (org "9.2.3"))
;; Version: 0.1

;;; Commentary:

;; Navigating through an org file to see what needs to be done
;; this week and what was completed last month can be tricky.
;; This tool provides a time window to analyse your org file.

;;; Code:

(require 'org)

(defgroup org-treescope nil
  "Group for setting org scope options"
  :prefix "org-treescope-"
  :group 'org)

(defcustom org-treescope--filename "projects.org"
  "Name of buffer to interact with."
  :type 'string
  :group 'org-treescope)

(defcustom org-treescope--modebinding "C-c m"
  "Binding to initialise org-treescope-mode"
  :type 'string
  :group 'org-treescope)

(define-minor-mode org-treescope-mode
  "Toggle the scoping options"
  :init-value nil
  :lighter " scope"
  :keymap
  '(([left] . org-treescope-timeleft)
    ([right] . org-treescope-timeright)
    ([up] . org-treescope-priorityup)
    ([down] . org-treescope-prioritydown)
    ([t] . org-treescope-prioritytoggleeq)
    ([return] . org-treescope-mode)))

(global-set-key (kbd org-treescope--modebinding) 'org-treescope-mode)

;; --- Interactives ----
(defun org-treescope-prioritytoggleeq ()
  "Changes direction of priority scope"
  (interactive)
  (setq org-treescope--currentpriorityeq
        (if (string-equal org-treescope--currentpriorityeq ">=") "<" ">="))
  (org-treescope-redraw))

(defun org-treescope-priorityup ()
  "Shifts priority threshold in scope up"
  (interactive)
  (let ((nextprior (- org-treescope--currentpriority 1)))
    (unless (< nextprior org-highest-priority)
      (setq org-treescope--currentpriority nextprior))
    (org-treescope-redraw)))

(defun org-treescope-prioritydown ()
  "Shifts priority threshold in scope down"
  (interactive)
  (let ((nextprior (+ org-treescope--currentpriority 1)))
    (unless (> nextprior org-lowest-priority)
      (setq org-treescope--currentpriority nextprior))
    (org-treescope-redraw)))

(defun org-treescope-timeleft ()
  "move left"
  (interactive)
  (org-treescope-time "-")
  (org-treescope-redraw))

(defun org-treescope-timeright ()
  "move right"
  (interactive)
  (org-treescope-time "+")
  (org-treescope-redraw))


;; --- Timescales ---
(defcustom org-treescope--timescales '("off" "1d" "3d" "7d" "2w" "1m" "3m" "6m" "9m" "1y")
  "The list of timescales to expand/shrink by"
  :type 'sequence
  :group 'org-treescope)
;;(setq org-treescope--timescales '("off" "1d" "3d" "7d" "2w" "1m" "3m" "6m" "9m" "1y"))
(setq org-treescope--currenttimescale (elt org-treescope--timescales 0))
(setq org-treescope--currentdirection "+")

(defun org-treescope-time (desireddirec)
  "moves the timescale in a direction sliding left or right"
  (let* ((d org-treescope--currentdirection)
         (ts org-treescope--currenttimescale)
         (allts org-treescope--timescales)
         (index (cl-position ts allts))
         (len (length allts)))
    (when (eq d "")
      (setq d desireddirec
            org-treescope--currentdirection desireddirec))
    (if (eq d desireddirec)
        ;; negative to more negative (i.e. we increase the index whilst
        ;; maintaining negativity
        (let ((nextindex (+ index 1)))
          (unless (>= nextindex len)
            (let ((nextts (nth nextindex allts)))
              (setq org-treescope--currenttimescale nextts))))
      ;; Positive to less positive "7d" "1d", or positive to negative "1d" to "-1d"
      (if (eq index 0)
          ;; pos to neg, the ts does not change, only direction
          ;; Switch direction and maintain magnitude if there is no nil
          (setq org-treescope--currentdirection desireddirec)
        ;; pos to less pos
        (let* ((nextindex (- index 1))
               (nextts (nth nextindex allts)))
          (setq org-treescope--currenttimescale nextts))))))

(progn (org-treescope-timeright)
       (concat org-treescope--currentdirection org-treescope--currenttimescale))


;; --- Priority management
(defcustom org-treescope--defaultpriority
  org-default-priority
  "Priority to start filtering for"
  :type 'integer
  :group 'org-treescope)
(defcustom org-treescope--priorityalwaysswitcheswithdirection
  t
  "Whether to change priority scopes when looking forward
or backwards. If false, then prioritylimit-forward and
backward have no effect."
  :type 'boolean
  :group 'org-treescope)

(setq org-treescope--currentpriorityeq ">=")
(setq org-treescope--currentpriority 67)


;; --- Construct Logic ---
(defun org-treescope-constructprioritystring ()
  "Creates the priority string"
  (let ((d org-treescope--currentdirection)
        (switches org-treescope--priorityalwaysswitcheswithdirection))
    (if switches
        ;; Need two separate statements depending on direction
        (if (eq d "+")
            (format "PRIORITY>=%d" org-treescope--currentpriority)
          (format "PRIORITY<%d" org-treescope--currentpriority))
      ;; Switch left to user
      (format "PRIORITY%s%d"
              org-treescope--currentpriorityeq
              org-treescope--currentpriority))))

(defcustom org-treescope--todostates
  '("TODO" "WAITING" "PAUSED")
  "List of TODO states to include in searches"
  :type 'sequence
  :group 'org-treescope)

(defun org-treescope--constructall ()
  "Generate the full string for the filtering"
  (let* ((d org-treescope--currentdirection)
         (off (string-equal org-treescope--currenttimescale "off"))
         (ineq (if (string-equal d "+") "<=" ">="))
         (dater (concat org-treescope--currentdirection
                        org-treescope--currenttimescale))
         (prior (org-treescope-constructprioritystring)))
    (if off prior
      ;; else
      (let ((todostates (concat "{" (join "\|" org-treescope--todostates) "}")))
        (concat 
         (format "DEADLINE%s\"<%s>\"&TODO=\"TODO\"+%s\|" ineq dater prior)
         (format "SCHEDULED%s\"<%s>\"&TODO=\"TODO\"+%s\|" ineq dater prior)
         (format "TIMESTAMP%s\"<%s>\"&TODO=\"TODO\"+%s" ineq dater prior))))))

(defun org-treescope-redraw ()
  "Redraw the whole thing" 
  (org-match-sparse-tree t (org-treescope--constructall))
  (message (format "<%s>+P%s%s"
                   (concat org-treescope--currentdirection
                           org-treescope--currenttimescale)
                   org-treescope--currentpriorityeq
                   org-treescope--currentpriority)))

(provide 'org-treescope)

;;; org-treescope.el ends here


;; test
;;(progn (org-treescope-timeleft)
;;       (concat org-treescope--currentdirection org-treescope--currenttimescale))
