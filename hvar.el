;;; hvar.el --- Variable manipulation routines for GNU Hyperbole  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     1-Oct-91 at 14:00:24
;; Last-Mod:      6-Aug-22 at 12:07:16 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'set)

;;; ************************************************************************
;;; Forward declarations
;;; ************************************************************************

(defvar inhibit-hyperbole-messaging) ; ; Defined in `hsettings' required below

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar var::append-list nil
  "List of (VAR-SYMBOL . APPENDED-LIST) elements saved from this Emacs session.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun var:add-and-run-hook (hook hook-function)
  "Add to mode HOOK the HOOK-FUNCTION; call it in matching major-mode buffers.
HOOK is a symbol whose name begins with a major-mode name and ends with
\"-hook\"."
  (add-hook hook hook-function)
  (let* ((hook-name (symbol-name hook))
	 (mode (when (string-match "-hooks?\\'" hook-name)
		 (intern (substring hook-name 0 (match-beginning 0))))))
    (when mode (var:run-hook-in-matching-buffers mode hook-function))))

(defun var:append-all ()
  "Add back all hook values previously added by var:append in this Emacs session.
The ones that were removed by `var:remove-all' at some point."
  (mapc (lambda (elt) (var:append (car elt) (cdr elt)))
	var::append-list)
  var::append-list)

;;;###autoload
(defun var:append (var-symbol list-to-add)
  "Append to value held by VAR-SYMBOL, LIST-TO-ADD.  Return new value.
If VAR-SYMBOL is unbound, it is set to LIST-TO-ADD.
Use to append to hook variables.  Store all values for later removal.
Do nothing when `inhibit-hyperbole-messaging' is non-nil."
  (unless (symbolp var-symbol)
    (error "(var:append): First argument, `%s', must be a symbol (not a string)" var-symbol))
  (unless (and list-to-add (listp list-to-add))
    (error "(var:append): Second argument, `%s', must be a non-empty list" list-to-add))
  (unless inhibit-hyperbole-messaging
    (let ((val) result)
      (setq result
	    (if (and (boundp var-symbol)
		     (setq val (symbol-value var-symbol))
		     (or (when (symbolp val)
			   (setq val (cons val nil)))
			 (listp val)))
		(progn (when (functionp val)
			 (setq val (list val)))
		       (set var-symbol (set:union val list-to-add)))
	      (set var-symbol list-to-add)))
      (add-to-list 'var::append-list (cons var-symbol result))
      (symbol-value var-symbol))))

(defun var:remove (var-symbol list-to-remove)
  "Remove from VAR-SYMBOL the functions in LIST-TO-REMOVE.
Use to remove from hook variables."
  (unless (symbolp var-symbol)
    (error "(var:remove): First argument, `%s', must be a symbol (not a string)" var-symbol))
  (unless (and list-to-remove (listp list-to-remove))
    (error "(var:remove): Second argument, `%s', must be a non-empty list" list-to-remove))
  (when (eq (car list-to-remove) 'lambda)
    (setq list-to-remove (list list-to-remove)))
  (mapc (lambda (func) (remove-hook var-symbol func))
	list-to-remove)
  (setq var::append-list (delete (cons var-symbol list-to-remove) var::append-list))
  (symbol-value var-symbol))

(defun var:remove-all ()
  "Remove all hook values added by `var:append' from their hook variables.
Affects only those hook values added by `var:append' in this Emacs session.
Keep a copy of these values for future re-use; see `var:append-all'."
  (mapc (lambda (elt) (var:remove (car elt) (cdr elt)))
	var::append-list)
  var::append-list)

(defun var:run-hook-in-matching-buffers (mode hook-function)
  "Within all buffers with a given major MODE, call HOOK-FUNCTION.
This is used after a hook is changed to affect buffers that
existed before the change was made."
  (mapc (lambda (buf) (with-current-buffer buf (funcall hook-function)))
	(delq nil (mapcar (lambda (buf) (when (eq (buffer-local-value 'major-mode buf) mode)
					  buf))
			  (buffer-list)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar var::append-list nil
  "List of (var-symbol . appended-list) elements saved from this Emacs session.")

(provide 'hvar)


;; `hsettings' and `hvar' have a cyclic dependency; require this after providing 'hvar
;; to avoid an infinite loop.
(require 'hsettings)

;;; hvar.el ends here
