;;; hpath-tests.el --- unit tests for hpath         -*- lexical-binding: t; -*-

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 28-Feb-21 at 23:26:00
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Unit tests for "../hpath.el"

;;; Code:

(require 'ert)
(require 'hpath)

(ert-deftest hpath:find-report-lisp-variable-path-name-when-not-exists ()
  "Test that hpath:find expands and returns filename when it is non-existent."
  (condition-case err
      (hpath:find "${hyperb:dir}/UNKNOWNFILE")
    (error (should (string-match (concat hyperb:dir "UNKNOWNFILE") (cadr err))))))

(provide 'hpath-tests)
;;; hpath-tests.el ends here
