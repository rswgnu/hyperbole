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

(ert-deftest hpath:load-modifier-loads-file ()
  "Path prefix - will load elisp file."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'load)
               (lambda (filename)
                 (setq was-called (should (string= "/folder/hyperbole.el" filename))))))
      (hpath:find "-/folder/hyperbole.el")
      (should was-called))))

(ert-deftest hpath:load-modifier-with-plain-file-loads-file-from-load-path ()
  "Path prefix - with filename without diretory will load from`load-path'."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'load)
               (lambda (filename)
                 (setq was-called
                       (should (string= (locate-library "tutorial.el") filename))))))
      (hpath:find "-tutorial.el")
      (should was-called))))

(ert-deftest hpath:subsitute-value-test ()
  "Environment and Lisp variables shall be substituted in a path."
  (progn ()
         (setq hypb:lc-var "lower")
         (setq hypb:uc-var "UPPER")
         (setenv "HYPB_TEST_ENV" "env")

         (should (string= (hpath:substitute-value "/nothing/to/substitute") "/nothing/to/substitute"))

         (should (string= (hpath:substitute-value "${hypb:lc-var}") hypb:lc-var))
         (should (string= (hpath:substitute-value "${hypb:uc-var}") hypb:uc-var))
         (should (string= (hpath:substitute-value "$HYPB_TEST_ENV") (getenv "HYPB_TEST_ENV")))
         (should (string= (hpath:substitute-value "${HYPB_TEST_ENV}") (getenv "HYPB_TEST_ENV")))

         (should (string= (hpath:substitute-value "prefix${hypb:lc-var}suffix") (concat "prefix" hypb:lc-var "suffix")))
         (should (string= (hpath:substitute-value "prefix/$HYPB_TEST_ENV/suffix") (concat "prefix/" (getenv "HYPB_TEST_ENV") "/suffix")))
         (should (string= (hpath:substitute-value "prefix${HYPB_TEST_ENV}suffix") (concat "prefix" (getenv "HYPB_TEST_ENV") "suffix")))

         (should (string= (hpath:substitute-value "${hypb:lc-var}${hypb:uc-var}") (concat hypb:lc-var hypb:uc-var)))
         (should (string= (hpath:substitute-value "$HYPB_TEST_ENV/$HYPB_TEST_ENV") (concat (getenv "HYPB_TEST_ENV") "/" (getenv "HYPB_TEST_ENV"))))
         (should (string= (hpath:substitute-value "${HYPB_TEST_ENV}${HYPB_TEST_ENV}") (concat (getenv "HYPB_TEST_ENV") (getenv "HYPB_TEST_ENV"))))

         (should (string= (hpath:substitute-value "prefix${hypb:lc-var}/$HYPB_TEST_ENV/suffix") (concat "prefix" hypb:lc-var "/" (getenv "HYPB_TEST_ENV") "/suffix")))

         (should (string= (hpath:substitute-value "$UNDEFINED_IS_NOT_SUBSTITUTED") "$UNDEFINED_IS_NOT_SUBSTITUTED"))
         (should (string= (hpath:substitute-value "${UNDEFINED_IS_NOT_SUBSTITUTED}") "${UNDEFINED_IS_NOT_SUBSTITUTED}"))
         ))

(provide 'hpath-tests)
;;; hpath-tests.el ends here
