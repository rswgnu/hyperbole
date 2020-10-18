;;; Allow use of Org minor-mode table editing in Koutlines
;;; If in an Org table, use kotl-mode {TAB} binding only when given an
;;; explicit prefix argument; otherwise, {TAB} acts normally in tables.

(require 'org-table)

;; Redefine this Org Table function to handle Koutlines as well.
(defun orgtbl-tab (arg)
  "Justification and field motion for `orgtbl-mode' with Hyperbole Koutline support."
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
  "Let Action Key handle tables in kotl-mode, otherwise, use standard
Org table command."
  (interactive "P")
  (if (derived-mode-p #'kotl-mode)
      (hkey-either arg)
    (org-table-wrap-region arg)))

(provide 'kotl-orgtbl)
