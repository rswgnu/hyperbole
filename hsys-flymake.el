;;; hsys-flymake.el --- Add missing source buffer keymap to flymake linter   -*- lexical-binding: t; -*-
;; Usage:        GNU Emacs Lisp Library
;; Keywords:     tools
;;
;; Author:       Bob Weiner
;; Org:          RWorks
;; E-mail:       rsw@gnu.org
;;
;; Orig-Date:    31-Dec-23 at 13:54:08
;; Last-Mod:      4-Jan-24 at 10:18:18 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2023-2024  Free Software Foundation, Inc.
;; Licensed under the GNU General Public License, version 3.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;    These functions all work in the source code buffer, not the flymake
;;    diagnostics buffer.
;;
;;    We want to load this so the implicit button type defined herein
;;    takes effect, but we don't require `flymake-mode' or
;;    `repeat-mode'.  Instead we leave it to each function to check
;;    whether it is enabled and active in the current buffer.  This way,
;;    if the user never uses `flymake-mode' that library is never loaded
;;    and the implicit button type never triggers.
;;
;;; Code:

(require 'hbut)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar flymake-mode)
(defvar flymake-wrap-around)
(defvar flymake-mode-map)

(declare-function flymake--diag-text "flymake")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defib hsys-flymake-issue-at-point ()
  "Display the flymake diagnostic issue at source buffer point, if any."
  (let ((issue (hsys-flymake-get-issue-at-position)))
    (when issue
      (ibut:label-set issue)
      (hact 'message issue))))

(defun hsys-flymake-issue-at-point:help (ibut)
  "With flymake enabled and point on its highlighted text, insert the issue."
  (interactive)
  (let ((issue (car (hattr:get ibut 'args))))
    (when issue
      (save-excursion
	(end-of-visible-line)
	(insert "\n" issue "\n")))))

(defun hsys-flymake-display-issue-at-point ()
  "Display the flymake diagnostic issue at source buffer point, if any."
  (interactive)
  (let ((issue (hsys-flymake-get-issue-at-position)))
    (when issue
      (message issue))))

(defun hsys-flymake-display-log-buffer ()
  "For flymake backend developers: Display a technical log of flymake actions."
  (interactive)
  (when (and (featurep 'flymake) flymake-mode)
    (display-buffer (get-buffer-create "*Flymake log*"))))

(defun hsys-flymake-get-issue-at-position (&optional pos)
  "With flymake enabled and POS on its highlighted text, return the issue.
If no issue at POS, return nil.  POS is optional and defaults to point."
  (when (and (featurep 'flymake) flymake-mode)
    (let* ((diag (get-char-property (or pos (point)) 'flymake-diagnostic))
	   (issue (when diag (flymake--diag-text diag))))
      issue)))

(defun hsys-flymake-display-this-or-next-issue ()
  "Display the source buffer flymake diagnostic at point or else the next one."
  (interactive)
  (or (hsys-flymake-display-issue-at-point)
      (call-interactively 'flymake-goto-next-error)))

(defun hsys-flymake-insert-issue-at-point ()
  "With flymake enabled and point on its highlighted text, insert the issue.
Issue is inserted into the buffer after the current visible line."
  (interactive)
  (let ((issue (hsys-flymake-get-issue-at-position)))
    (when issue
      (save-excursion
	(end-of-visible-line)
	(insert "\n" issue "\n")))))

(defun hsys-flymake-toggle ()
  "Toggle flymake minor mode on or off in the current buffer."
  (interactive)
  (flymake-mode 'toggle)
  (when (called-interactively-p 'interactive)
    (message "%s flymake-mode" (if flymake-mode "Enabled" "Disabled"))))

(defun hsys-flymake-toggle-wraparound ()
  "Toggle flymake minor mode on or off in the current buffer."
  (interactive)
  (when (and (featurep 'flymake) flymake-mode)
    (setq flymake-wrap-around (not flymake-wrap-around))
    (when (called-interactively-p 'interactive)
      (message "%s flymake-mode next and previous issue search wraparound to buffer start"
	       (if flymake-wrap-around "Enabled" "Disabled")))))

;;; ************************************************************************
;;; Key bindings
;;; ************************************************************************

;; flymake-mode does not bind any keys; it has only menu bindings.
;; Add key bindings for use in the source buffer on the C-c C-l prefix.
;; Each time these keys are changed, need to disable repeat-mode and
;; then re-enable it to read the updated key bindings.
(defvar hsys-flymake-mode-control-l-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'flymake-show-buffer-diagnostics)
    (define-key map "g" 'hsys-flymake-display-this-or-next-issue)
    (define-key map "i" 'hsys-flymake-insert-issue-at-point)
    (define-key map "l" 'flymake-switch-to-log-buffer)
    (define-key map "n" 'flymake-goto-next-error)
    (define-key map "p" 'flymake-goto-prev-error)
    (define-key map "s" 'flymake-start)
    (define-key map "w" 'hsys-flymake-toggle-wraparound)
    map))

;; Use repeat-mode for appropriate flymake commands so can repeat them
;; with their last keystroke, e.g. repeat {C-c C-l n} by pressing {n}
;; repeatedly.
(defvar hsys-flymake-repeat-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (cmd)
	    (define-key map
	      (where-is-internal cmd hsys-flymake-mode-control-l-prefix-map t)
	      cmd)
	    (put cmd 'repeat-map 'hsys-flymake-repeat-map))
	  '(flymake-goto-next-error
	    flymake-goto-prev-error
	    hsys-flymake-toggle-wraparound))
    map))

(defvar hsys-flymake-mode-control-c-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l" hsys-flymake-mode-control-l-prefix-map)
    map))

(global-set-key "\C-c\C-lt" 'hsys-flymake-toggle)

(add-hook 'flymake-mode-hook
	  (lambda () (when flymake-mode
		       (define-key flymake-mode-map "\C-c"
			 hsys-flymake-mode-control-c-prefix-map)
		       (repeat-mode 1))))
;; If flymake-mode is already enabled, re-enable it to ensure
;; repeat-mode gets enabled.
(when (and (featurep 'flymake) flymake-mode)
  (flymake-mode 1))

(provide 'hsys-flymake)

;;; hsys-flymake.el ends here
