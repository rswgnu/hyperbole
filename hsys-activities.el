;;; hsys-activities.el --- action button support for activities -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     7-Dec-25 at 22:48:29
;; Last-Mod:     11-Dec-25 at 22:26:21 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Use as action button <hsys-activity "Activity">
;;
;; Create "Activity" if it does not exist.  If "Activity" is not
;; active, switch to its latest state.  If "Activity" is active,
;; revert it to its default state.  If "Activity" is active and the
;; action button is called with a prefix argument a new default state
;; is set.

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'hypb)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar activities-name-prefix)

(declare-function activities-current "ext:activities")
(declare-function activities-define "ext:activities")
(declare-function activities-name-for "ext:activities")
(declare-function activities-named "ext:activities")
(declare-function activities-names "ext:activities")
(declare-function activities-resume "ext:activities")
(declare-function activities-revert "ext:activities")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hsys-activities (name)
  "Create, resume and revert activity NAME in one function.
- Create activity with NAME if it does not exist.
- If activity NAME is not active, switch to its latest state.
- If activity NAME is active and current, revert to its default state.
- If activity NAME is active and hsys-activity is called with
  `current-prefix-arg' set then set the default state."
  (interactive (list (completing-read "Activity: " (activities-names) nil nil)))
  (hypb:require-package 'activities)
  (let ((activity (activities-named name)))
    (cond ((not activity)
           (activities-define name)
           (message "Activity %s defined." name))
          ((let ((current-activity (activities-current)))
             (and current-activity
                  (string=
                   (activities-name-for activity)
                   (activities-name-for current-activity))))
           (if current-prefix-arg
               (progn
                 (activities-define name :forcep t)
                 (message "Activity %s set to new default." name))
             (activities-revert activity)
             (message "Activity %s reverted." name)))
          (t
           (activities-resume activity)
           (message "Activity %s resumed." name)))))

(provide 'hsys-activities)
;;; hsys-activities.el ends here
