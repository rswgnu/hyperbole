;;; hy-test-coverage.el --- support for test coverage     -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    21-Mar-24 at 13:22:27
;; Last-Mod:      7-Apr-24 at 10:43:42 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Uses the testcover functionality and runs a specified test
;; suite for a file that is monitored for coverage.  See
;; "testcover.el" for how to interpret the "splotches", the color code
;; characters in the monitored filed.
;;
;; See also "../Makefile#coverage:", a make target for running from the
;; command line.

;;; Code:

(require 'hypb-ert)
(require 'testcover)

(defun hy-test--count-coverage ()
  "Count coverage splotches."
  (cl-count-if
   (lambda (x)
     (string-prefix-p "testcover-" (symbol-name (overlay-get x 'face))))
   (car (overlay-lists))))

(defun hy-test-coverage-file (filename &optional testspec)
  "In FILENAME, run TESTSPEC and produce coverage data.
With no TESTSPEC all tests are run."
  (interactive "fFilename: \nsTestspec: ")
  (unless (file-exists-p filename)
    (error "(hy-test-coverage-file): File %s does not exist" filename))
  (unless testspec
    (setq testspec t))
  (let ((buff (find-file filename)))
    (testcover-unmark-all buff)
    (hypb-ert-require-libraries)
    (testcover-start filename)
    (bury-buffer)
    (ert testspec)
    (testcover-mark-all buff)
    (message "Number of splotches %d." (hy-test--count-coverage))
    (switch-to-buffer buff)
    (point-min)))

(provide 'hy-test-coverage)
;;; hy-test-coverage.el ends here
