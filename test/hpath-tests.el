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

(ert-deftest hpath:path-at-point-in-path-variable-test ()
  "Find path at point in path variable."
  (with-temp-buffer
    (insert "\":foo:bar:emacs\"")
    (goto-char 8)
    (should (string= (hpath:at-p) "bar"))))

(ert-deftest hpath:path-at-point-in-path-variable-shorter-than-three-colons-returns-nil-test ()
  "Do not identify path variables with less than three colons."
  (with-temp-buffer
    (insert "\"foo:bar:lisp\"")
    (goto-char 7)
    (should (not (hpath:at-p)))))

(ert-deftest hpath:find-exec-shell-cmd-test ()
  "Path prefix ! will run pathname as a non windowed program."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'actypes::exec-shell-cmd)
               (lambda (filename)
                 (setq was-called (should (string= "/bin/ls" filename))))))
      (hpath:find "!/bin/ls")
      (should was-called))))

(ert-deftest hpath:find-exec-window-cmd-test ()
  "Path prefix & will run pathname as a windowed program."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'actypes::exec-window-cmd)
               (lambda (filename)
                 (setq was-called (should (string= "/bin/ls" filename))))))
      (hpath:find "&/bin/ls")
      (should was-called))))


(provide 'hpath-tests)
;;; hpath-tests.el ends here
