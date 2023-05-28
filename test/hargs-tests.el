;;; hargs-tests.el --- Tests for hargs.el                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    04-Feb-22 at 23:00:00
;; Last-Mod:     28-May-23 at 23:14:18 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Tests for "../hargs.el"

;;; Code:

(require 'ert)
(require 'with-simulated-input)
(require 'hargs)
(require 'hy-test-helpers "test/hy-test-helpers")

(ert-deftest hargs-get-verify-extension-characters ()
  "Verify hyperbole extension characters are indentified."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (with-simulated-input "xyz RET"
            (should (string= (hargs:get "+I: ") "xyz")))
          (with-simulated-input "xyz RET"
            (should (string= (hargs:get "+L: ") "xyz")))
          (with-simulated-input '((insert "xyz" file) "RET")'
            (should (equal (hargs:get "+M: ") (list "xyz" file))))
          (with-simulated-input "xyz RET"
            (should (string= (hargs:get "+V: ") "xyz")))
          (with-simulated-input "xyz RET"
            (should (string= (hargs:get "+X: ") "(dir)xyz")))
          (should-error (hargs:get "+A: ") :type 'error))
      (hy-delete-file-and-buffer file))))

(ert-deftest hargs-get-verify-extension-characters-+K ()
  "Verify hyperbole extension character +K is indentified."
  (cl-letf (((symbol-function 'hargs:read-match) (lambda (prompt a &optional b c d e) "xyz"))
	    ((symbol-function 'kview:map-tree) (lambda (a b c d) nil))
	    ((symbol-function 'kcell-view:visible-label) (lambda () nil)))
    (should (string= (hargs:get "+K: ") "xyz"))))

;; This file can't be byte-compiled without `with-simulated-input' which
;; is not part of the actual dependencies, so:
;;   Local Variables:
;;   no-byte-compile: t
;;   End:

(provide 'hargs-tests)
;;; hargs-tests.el ends here
