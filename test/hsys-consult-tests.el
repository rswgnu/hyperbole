;;; hsys-consult-tests.el --- unit tests for hsys-consult     -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    14-Jul-24 at 19:00:28
;; Last-Mod:     14-Jul-24 at 19:21:04 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'hsys-consult)
(require 'ert)
(require 'el-mock)

(ert-deftest hsys-consult--grep-version-dependencies ()
  "Check consult version dependency is caught."
  (mocklet ((hsys-consult-get-version => "0.32"))
    (let ((err (should-error (hsys-consult-grep "includes" "globs") :type 'error)))
      (should
       (string-match-p
        "(hsys-consult-grep): consult package version is 0.32; update required"
        (cadr err)))))
  (mocklet ((hsys-consult-get-version => "0.33")
            (hsys-consult--grep-paths => t))
    (hsys-consult-grep "includes" "globs")))

(provide 'hsys-consult-tests)
;;; hsys-consult-tests.el ends here
