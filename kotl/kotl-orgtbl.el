;;; kotl-orgtbl.el --- Allow use of Org minor-mode table editing in koutlines  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    10/18/2020
;; Last-Mod:     24-Jan-22 at 00:25:29 by Bob Weiner
;;
;; Copyright (C) 2020-2021  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;   - Org Table Support: Org table editing now automatically works in the
;;     Koutliner via Org table minor mode.  Use {M-x orgtbl-mode RET} to
;;     toggle this on and off or simply press the Action Key on a pipe |
;;     character to do the same thing.  See "(Org)Tables" for details.

;;   - New Tree Demotion/Promotion Keys: Tree promotion and demotion keys now
;;     match the defaults in Org mode and Outline mode, plus some easier to
;;     type ones.  The tables below summarize which keys work whether inside
;;     an Org table or outside.

;;     |----------------------------+-----------------------------|
;;     | Demotion Inside Org Table  | Promotion Inside Org Table  |
;;     |----------------------------+-----------------------------|
;;     | C-c C-,                    | C-c C-.                     |
;;     | C-c C-<                    | C-c C->                     |
;;     |----------------------------+-----------------------------|

;;     |----------------------------+-----------------------------|
;;     | Demotion Outside Org Table | Promotion Outside Org Table |
;;     |----------------------------+-----------------------------|
;;     | TAB                        | Shift-TAB or M-TAB          |
;;     | M-left-arrow               | M-right-arrow               |
;;     | C-c C-,                    | C-c C-.                     |
;;     | C-c C-<                    | C-c C->                     |
;;     |----------------------------+-----------------------------|

;;  If in an Org table, the kotl-mode {TAB} binding operates only
;;  when invoked with an explicit prefix argument; otherwise, {TAB}
;;  performs its normal table-based alignment and movement.

;;; Code:

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'org-table)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Redefine this Org Table function to handle Koutlines as well.
(defun orgtbl-tab (arg)
  "Justification and field motion for `orgtbl-mode' with Koutline support."
  (interactive "P")
  (cond ((and (derived-mode-p #'kotl-mode) arg)
	 (kotl-mode:tab-command (if (= (prefix-numeric-value arg) 1) nil arg)))
	(arg
	 (org-table-edit-field t))
	(t (org-table-justify-field-maybe)
	   (org-table-next-field))))

;; !! TODO: Doesn't leave point in the same place of orig line
(defun kotl-mode:transpose-lines-up ()
  "Exchange current line and previous line, maintaining point location.
If no previous line, exchange current with next line."
  (interactive)
  (let ((opoint (set-marker (make-marker) (point))))
    (kotl-mode:transpose-lines 1)
    (goto-char opoint)
    (set-marker opoint nil)))


(defun kotl-mode:transpose-lines-down ()
  (interactive)
  ;; !! TODO: Write
  )

(defun orgtbl-meta-return (arg)
  "Let Action Key handle tables in kotl-mode, otherwise, use Org table command."
  (interactive "P")
  (if (derived-mode-p #'kotl-mode)
      (hkey-either arg)
    (org-table-wrap-region arg)))

(provide 'kotl-orgtbl)

;;; kotl-orgtbl.el ends here
