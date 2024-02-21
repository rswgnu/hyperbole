;;; kimport-tests.el --- test importing file types to Koutlines    -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     9-Apr-23 at 23:31:48
;; Last-Mod:     22-Feb-24 at 00:02:33 by Mats Lidell
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
(require 'hy-test-helpers "test/hy-test-helpers")

(ert-deftest kimport--aug-post-outline ()
  "Import .otl star outline as one cell per entry beginning with one or more stars."
  (let ((file (make-temp-file "hypb" nil ".aug"
			      (concat "entry1  1\n\nentry1a  1a\n\nentry1b  1b\n\n"
				      "entry2  2\n\nentry3  3\n\nentry3a  3a\n")))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '("entry1" "entry1a" "entry1b" "entry2" "entry3" "entry3a"))
            (should (looking-at-p v))
	    (should (string-suffix-p (kcell-view:label) v))
	    (unless (kotl-mode:last-cell-p)
              (kotl-mode:next-cell 1)))
	  (should (kotl-mode:last-cell-p)))
      (hy-delete-file-and-buffer file)
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kimport--text-file ()
  "Import .txt text file into a Koutline, as one cell per paragraph."
  (let ((file (make-temp-file "hypb" nil ".txt" "1\n\n2\n\n3\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '(1 2 3))
            (should (looking-at-p (number-to-string v)))
	    (unless (kotl-mode:last-cell-p)
              (kotl-mode:forward-cell 1)))
	  (should (kotl-mode:last-cell-p)))
      (hy-delete-file-and-buffer file)
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kimport--text-file-two-lines-per-paragraph ()
  "Import .txt text file into a Koutline, as one cell per paragraph.
Each paragraph is two lines."
  (let ((file (make-temp-file "hypb" nil ".txt"
			      (concat "par1 line1\npar1 line2\n\n\npar2 line3\n par2 line4"
				      "\n\n par3 *. line5\n par3 *. line6")))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '("par1 line1\\s-+par1 line2" "par2 line3\\s-+par2 line4"
		       "par3 \\*\\. line5\\s-+par3 \\*\\. line6"))
            (should (looking-at-p v))
	    (unless (kotl-mode:last-cell-p)
              (kotl-mode:forward-cell 1)))
	  (should (kotl-mode:last-cell-p)))
      (hy-delete-file-and-buffer file)
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kimport--star-outline ()
  "Import .otl star outline as one cell per entry beginning with one or more stars."
  (let ((file (make-temp-file "hypb" nil ".otl" "* 1\n** 1a\n** 1b\n* 2\n* 3\n** 3a\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '("1" "1a" "1b" "2" "3" "3a"))
            (should (looking-at-p v))
	    (should (equal (kcell-view:label) v))
	    (unless (kotl-mode:last-cell-p)
              (kotl-mode:next-cell 1)))
	  (should (kotl-mode:last-cell-p)))
      (hy-delete-file-and-buffer file)
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kimport--star-outline-two-lines-per-star-heading ()
  "Import .org star outline as one cell per paragraph, each two lines."
  (let ((file (make-temp-file "hypb" nil ".org" "* 1\n2\n* 3\n4\n* 5\n6\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '("1\n.*2" "3\n.*4" "5\n.*6"))
            (should (looking-at-p v))
	    (unless (kotl-mode:last-cell-p)
              (kotl-mode:forward-cell 1)))
	  (should (kotl-mode:last-cell-p)))
      (hy-delete-file-and-buffer file)
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kimport--star-outline-with-siblings ()
  "Import .org star outline as one cell per entry beginning with one or more stars."
  (let ((file (make-temp-file "hypb" nil ".org" "* 1\n** 2\n*** 3\n"))
        (kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (kimport:file file kotl-file)
          (find-file kotl-file)
          (dolist (v '(1 2))
            (should (looking-at-p (number-to-string v)))
	    (unless (kotl-mode:last-cell-p)
              (kotl-mode:next-cell 1)))
          (should (looking-at-p (number-to-string 3)))
	  (should (kotl-mode:last-cell-p))
          (kotl-mode:end-of-buffer)
          (should (= (kcell-view:level) 3)))
      (hy-delete-file-and-buffer file)
      (hy-delete-file-and-buffer kotl-file))))

(provide 'kimport-tests)
;;; kimport-tests.el ends here
