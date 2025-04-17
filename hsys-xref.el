;;; hsys-xref.el --- GNU Hyperbole support functions for "xref.el"  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    24-Aug-91
;; Last-Mod:     14-Apr-25 at 15:51:27 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1991-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'xref)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function smart-emacs-lisp-mode-p "hmouse-tag")
(declare-function hpath:at-p "hpath")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hsys-xref-definitions (identifier)
  "Return a list of all definitions of string IDENTIFIER."
  (let* ((elisp-flag (smart-emacs-lisp-mode-p t))
	 (xref-backend (or (and elisp-flag
				(fboundp 'ert-test-boundp)
				(ert-test-boundp (intern-soft identifier))
				(boundp 'xref-etags-mode)
				'etags)
			   (xref-find-backend)))
	 (xref-items (xref-backend-definitions xref-backend identifier)))
    xref-items))

(defun hsys-xref-definition (identifier)
  "Return the first definition of string IDENTIFIER."
  (car (hsys-xref-definitions identifier)))

(defun hsys-xref-identifier-at-point ()
  "Return nil if xref returns a pathname as an identifier."
  (unless (hpath:at-p nil t)
    (xref-backend-identifier-at-point (xref-find-backend))))

(defun hsys-xref-item-buffer (item)
  "Return the buffer in which xref ITEM is defined."
  (marker-buffer (save-excursion (xref-location-marker (xref-item-location item)))))

(defun hsys-xref-item-position (item)
  "Return the buffer position where xref ITEM is defined."
  (marker-position (save-excursion (xref-location-marker (xref-item-location item)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun xref--item-at-point ()
  "Fix next xref function to handle when called at beginning of buffer."
  (get-text-property
   (max (point-min) (if (eolp) (1- (point)) (point)))
   'xref-item))

(defalias 'hsys-xref-item-at-point #'xref--item-at-point)

(provide 'hsys-xref)

;;; hsys-xref.el ends here
