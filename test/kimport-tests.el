;;; kimport-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     9-Apr-23 at 23:31:48
;; Last-Mod:     11-Apr-23 at 00:59:17 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'kimport)
(require 'ert)

(ert-deftest kimport--text-file ()
  "Import text file as one cell per paragraph."
  (skip-unless false) ;; Skip until text file import if fixed
  (let ((file (make-temp-file "hypb" nil ".txt" "1\n\n2\n\n3\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '(1 2 3))
            (should (looking-at-p (number-to-string v)))
            (kotl-mode:forward-cell 1)))
      (delete-file file)
      (delete-file kotl-file))))

(ert-deftest kimport--text-file-two-lines-per-paragraph ()
  "Import text file as one cell per paragraph each two lines."
  (skip-unless false) ;; Skip until text file import if fixed
  (let ((file (make-temp-file "hypb" nil ".txt" "1\n2\n\n\n3\n4\n\n5\n6\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '("1\n.*2" "3\n.*4" "5\n.*6"))
            (should (looking-at-p v))
            (kotl-mode:forward-cell 1)))
      (delete-file file)
      (delete-file kotl-file))))

(ert-deftest kimport--star-outline ()
  "Import star outline as one cell per star."
  (let ((file (make-temp-file "hypb" nil ".org" "* 1\n* 2\n* 3\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '(1 2 3))
            (should (looking-at-p (number-to-string v)))
            (kotl-mode:forward-cell 1)))
      (delete-file file)
      (delete-file kotl-file))))

(ert-deftest kimport--star-outline-two-lines-per-star-heading ()
  "Import text file as one cell per paragraph each two lines."
  (let ((file (make-temp-file "hypb" nil ".org" "* 1\n2\n* 3\n4\n* 5\n6\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '("1\n.*2" "3\n.*4" "5\n.*6"))
            (should (looking-at-p v))
            (kotl-mode:forward-cell 1)))
      (delete-file file)
      (delete-file kotl-file))))

(ert-deftest kimport--star-outline-with-siblings ()
  "Import star outline as one cell per star."
  (let ((file (make-temp-file "hypb" nil ".org" "* 1\n** 2\n*** 3\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '(1 2))
            (should (looking-at-p (number-to-string v)))
            (kotl-mode:next-cell 1))
          (should (looking-at-p (number-to-string 3)))
          (kotl-mode:end-of-buffer)
          (should (= (kcell-view:level) 3)))
      (delete-file file)
      (delete-file kotl-file))))

(provide 'kimport-tests)
;;; kimport-tests.el ends here
