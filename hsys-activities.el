;;; hsys-activities.el --- action button support for activities -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     7-Dec-25 at 22:48:29
;; Last-Mod:      7-Dec-25 at 23:14:18 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  Activate an activity:
;;    <hsys-activities-resume "Activity">
;;  Activate an activity to its default configuration:
;;    <hsys-activities-resume "Activity" t>
;;  Revert current activity to its default configuration:
;;    <hsys-activities-resume "Activity">

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'hypb)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function activities-named "ext:activities")
(declare-function activities-resume "ext:activities")
(declare-function activities-current "ext:activities")
(declare-function activities-revert "ext:activities")
(declare-function activities-name-for "ext:activities")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hsys-activities-resume (name &optional reset)
  "Resume an activity given by NAME.
If non-nil RESET activity's back to default configuration."
  (hypb:require-package 'activities)
  (let ((activity (activities-named name)))
    (unless activity
      (user-error "No activity: %s" name))
    (activities-resume activity :resetp reset)
    (message "Activity: %s." name)))

(defun hsys-activities-revert ()
  "Revert current activity to its default configuration."
  (hypb:require-package 'activities)
  (let ((activity (activities-current)))
    (activities-revert activity)
    (message "Reverted: '%s'." (activities-name-for activity))))

(provide 'hsys-activities)
;;; hsys-activities.el ends here
