;;; hbut-tests.el --- hbut unit tests         -*- lexical-binding: t; -*-

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 30-may-21 at 09:33:00
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; "../hbut.el"

;;; Code:

(require 'ert)
(require 'hbut)
(require 'hactypes)
(require 'el-mock)

(ert-deftest ebut-program-link-to-directory ()
  "Programatically create ebut with link-to-directory."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory "/tmp")
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
          (should (equal (hattr:get (hbut:at-p) 'args) '("/tmp")))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest ebut-program-link-to-directory-2 ()
  "Programatically create ebut with link-to-directory."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory (temporary-file-directory))
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
          (should (equal (hattr:get (hbut:at-p) 'args) '((temporary-file-directory))))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest ebut-program-shell-cmd ()
  "Programatically create ebut running shell command."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'exec-shell-cmd "ls /tmp")
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::exec-shell-cmd))
          (should (equal (hattr:get (hbut:at-p) 'args) '("ls /tmp")))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest ebut-delete-removes-ebut-and-returns-button-data ()
  "Remove a ebut."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory "/tmp")
          (should (hbut:at-p))
          (let ((args (hbut:delete)))
            (should-not (hbut:at-p))
            (should (equal (hbdata:actype args) "actypes::link-to-directory"))
            (should (equal (hbdata:args args) '("/tmp")))
            ))
      (delete-file file))))

(ert-deftest gbut-program-calls-ebut-program ()
  "Programatically create gbut calls ebut:program."
  (let ((test-file (make-temp-file "test-file")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
        (with-mock
          (mock (find-file-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
          (mock (ebut:program "label" 'actypes::link-to-directory "/tmp") => t)
          (gbut:ebut-program "label" 'actypes::link-to-directory "/tmp"))
      (delete-file test-file))))

(ert-deftest gbut-program-link-to-directory ()
  "Programatically create gbut with link-to-directory."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (find-file-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'actypes::link-to-directory "/tmp"))
	  (with-current-buffer test-buffer
            (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
            (should (equal (hattr:get (hbut:at-p) 'args) '("/tmp")))
            (should (equal (hattr:get (hbut:at-p) 'lbl-key) "global"))))
      (delete-file test-file))))

(provide 'hbut-tests)
;;; hbut-tests.el ends here
