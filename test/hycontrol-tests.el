;;; hycontrol-tests.el --- verify hycontrol -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     8-Jan-25 at 22:52:00
;; Last-Mod:      8-Jan-25 at 23:13:27 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Verify functions and features provided by "hycontrol.el".

;;; Code:

(require 'hycontrol)
(require 'ert)
(require 'el-mock)

(ert-deftest hycontrol-tests--framemove-direction-error-message ()
  "Verify `hycontrol-framemove-direction' shows message when `framemove' is not available."
  (mocklet (((featurep 'framemove) => nil)
            ((hycontrol-quit) => t))
    (let ((err (should-error (hycontrol-framemove-direction 'up) :type 'error)))
      (should (string-match "Requires manual installation" (cadr err))))))

(provide 'hycontrol-tests)
;;; hycontrol-tests.el ends here