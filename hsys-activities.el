;;; hsys-activities.el --- actype for external package activities -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    21-Sep-25 at 20:41:48
;; Last-Mod:     28-Sep-25 at 23:56:48 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Stores the name of the activity rather than the activity itself.
;;
;; This actype resumes the activity.  For resetting the default state
;; of the activity, use `activites-resume' with a prefix argument.
;; With the default key binding for activities this is: {C-u C-x C-a g}.

;;; Code:

(require 'hypb)
(declare-function activities-activity-name "ext:activities")
(declare-function activities-completing-read "ext:activities")
(declare-function activities-named "ext:activities")
(declare-function activities-resume "ext:activities")

(defun hsys-activities-read ()
  "Prompt user for activity and return the activity name."
  (hypb:require-package 'activities)
  (activities-activity-name (activities-completing-read :prompt "Activity")))

(defact activity (activity)
  "Action type for activities."
  (interactive (list (hsys-activities-read)))
  (hypb:require-package 'activities)
  (when (stringp activity)
    (activities-resume (activities-named activity))))

(provide 'hsys-activities)
;;; hsys-activities.el ends here
