;;; hypb-ert-tests.el --- tests for hypb-ert                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     1-Jan-24 at 23:11:54
;; Last-Mod:     21-Mar-24 at 10:52:53 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'hy-test-dependencies) ;; can install el-mock
(require 'hypb-ert)

(ert-deftest hypb-ert-tests--def-at-p ()
  "Verify an `ert-deftest' name is identified."
  (let ((test-name "hypb-ert-tests--test"))
    (with-temp-buffer
      (insert "(ert-deftest " test-name " ()\n\"Docstring.\"\nt\n)\n")

      (goto-char (point-min))
      (should-not (hypb-ert-def-at-p))

      (goto-char (1+ (point-min)))
      (should (string= (hypb-ert-def-at-p) test-name))

      (goto-char (- (line-end-position) 3))
      (should (string= (hypb-ert-def-at-p) test-name))

      (goto-char (- (line-end-position) 2))
      (should-not (hypb-ert-def-at-p))

      (end-of-line)
      (should-not (hypb-ert-def-at-p))

      (goto-char (1+ (point-min)))
      (pcase-let ((`(,name ,start ,end)
                   (hypb-ert-def-at-p t)))
        (should (string= name test-name))
        (should (string= (buffer-substring start end) test-name))))))

(ert-deftest hypb-ert-tests--edebug-is-called ()
  "Verify `edebug-defun' is called when debug-it argument is set."
  (let ((test-name "hypb-ert-tests--test"))
    (with-temp-buffer
      (insert "(ert-deftest " test-name " ()\n\"Docstring.\"\nt\n)\n")
      (emacs-lisp-mode)
      (goto-char (1+ (point-min)))
      (mocklet (((hypb-ert *) => t))
        (hypb-ert-run-test-at-definition t)))))

(provide 'hypb-ert-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hypb-ert-tests.el ends here
