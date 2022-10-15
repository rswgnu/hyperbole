;;; hyrolo-demo.el --- Code to support DEMO introduction to HyRolo  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     4-Nov-17 at 13:56:47
;; Last-Mod:      7-Oct-22 at 00:16:24 by Mats Lidell
;;
;; Copyright (C) 2017-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'hyrolo-logic)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hyrolo-demo-save-key nil)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hyrolo-demo-fgrep (string &optional max-matches)
  "Display rolo entries in \"DEMO-ROLO.otl\" matching STRING or a logical sexp.
Display to a maximum of optional prefix arg MAX-MATCHES.
Each entry is displayed with all of its sub-entries.

Nil value of MAX-MATCHES means find all matches, t value means find all
matches but omit file headers, negative values mean find up to the inverse of
that number of entries and omit file headers.

Returns number of entries matched.  See also documentation for
the function `hyrolo-demo-fgrep-logical' for documentation on the
logical sexpression matching."
  (interactive "sFind rolo string (or logical sexpression): \nP")
  (let ((hyrolo-file-list (list (expand-file-name "DEMO-ROLO.otl" hyperb:dir))))
    (hyrolo-fgrep string max-matches)))

;;;###autoload
(defun hyrolo-demo-fgrep-logical (expr &optional count-only include-sub-entries no-sub-entries-out)
  "Display rolo entries in \"DEMO-ROLO.otl\" matching EXPR.
EXPR may contain prefix logical operators.
If optional COUNT-ONLY is non-nil, don't display entries, return
count of matching entries only.  If optional INCLUDE-SUB-ENTRIES
flag is non-nil, SEXP will be applied across all sub-entries at
once.  Default is to apply SEXP to each entry and sub-entry
separately.  Entries are displayed with all of their sub-entries
unless INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT
flag is non-nil.

A complex example of EXPR might be:
  (and (or (not time card) (xor (french balloons) spanish)) teacher pet)
which means:
  Match neither `time' nor `card'
    or
  Matches exactly one of `french balloons' or `spanish'
    and
  Matches `teacher' and `pet'.

Either double quotes or parentheses may be used to group multiple words as a
single argument."
  (interactive "sLogical rolo search: \nP\nP")
  (when (called-interactively-p 'any)
    (setq no-sub-entries-out (not no-sub-entries-out)))
  (let ((hyrolo-file-list (list (expand-file-name "DEMO-ROLO.otl" hyperb:dir))))
    (hyrolo-fgrep-logical expr count-only include-sub-entries no-sub-entries-out)))

(defun hyrolo-demo-quit ()
  "Remove the code in this file."
  (interactive)
  (when hyrolo-demo-save-key
    (global-set-key "\C-x4r" hyrolo-demo-save-key))
  (makunbound 'hyrolo-demo-save-key)
  (fmakunbound 'hyrolo-demo-fgrep)
  (fmakunbound 'hyrolo-demo-fgrep-logical)
  (setq features (delq 'hyrolo-demo features))
  (mapc (lambda (buf) (when (get-buffer buf) (kill-buffer buf)))
	'("*Hyperbole Rolo*" "DEMO-ROLO.otl"))
  (load "hyperbole-autoloads")
  (fmakunbound 'hyrolo-demo-quit)
  (message "HyRolo demo code removed and {C-x 4 r} key binding reset."))

;;; ************************************************************************
;;; Key Bindings
;;; ************************************************************************

(unless (eq (key-binding "\C-x4r") #'hyrolo-demo-fgrep)
  (setq hyrolo-demo-save-key (key-binding "\C-x4r")))

(global-set-key "\C-x4r" 'hyrolo-demo-fgrep)

(provide 'hyrolo-demo)
;;; hyrolo-demo.el ends here
