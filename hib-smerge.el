;;; hib-smerge.el --- ibut for acting on merge buffers -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    17-Apr-25 at 21:50:59
;; Last-Mod:     17-Apr-25 at 22:47:25 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'hbut)
(require 'smerge-mode)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defib hypb-smerge ()
  "Act on conflicts in merge buffers, i.e. when smerge-mode is active."
  (when (bound-and-true-p smerge-mode)
    (let (op)
      (save-excursion
        (beginning-of-line)
        (cond ((looking-at smerge-end-re)
               (setq op 'smerge-keep-lower))
              ((looking-at smerge-begin-re)
               (setq op 'smerge-keep-upper))
              ((looking-at smerge-lower-re)
               (setq op 'smerge-keep-all))))
      (when op
        (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0))
        (hact op)))))

(provide 'hib-smerge)
;;; hib-smerge.el ends here
