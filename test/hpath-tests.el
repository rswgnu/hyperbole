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
  "hpath:find prints out the wrong filename on lisp variable expanded filenames"
  :expected-result :failed
  (condition-case err
      (hpath:find "${hyperb:dir}/UNKNOWNFILE")
    (error (should (string-search (concat hyperb:dir "UNKNOWNFILE") (cadr err))))))

(provide 'hpath-tests)
;;; hpath-tests.el ends here
