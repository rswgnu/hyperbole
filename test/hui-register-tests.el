;;; hui-register-tests.el --- test for hui-register         -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    10-Sep-22 at 20:43:17
;; Last-Mod:     22-Feb-24 at 00:00:12 by Mats Lidell
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

(require 'ert)
(require 'hmouse-drv)
(require 'hui-register)
(require 'hy-test-helpers "test/hy-test-helpers")

(ert-deftest hui-register-test--create-register-content ()
  "Verify the struct contains its parts."
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[label label]> $HOME")
          (goto-char 5)
          (let ((content (hui-register-struct-at-point)))
            (should (equal (hui-register-but-label content) "label_label"))
            (should (equal (hui-register-but-link content) 'link-to-ibut))
            (should (markerp (hui-register-but-mpos content)))
            (should (equal (marker-buffer (hui-register-but-mpos content)) (current-buffer)))
            (should (equal (hui-register-but-file content) (hypb:buffer-file-name)))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-register-test--register-val-jump-to ()
  "Verify register val jumps to right file."
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[label]> $HOME")
          (goto-char 5)
          (let ((content (hui-register-struct-at-point))
                (pos (point)))
            (set-buffer "*scratch*")
            (should (equal (buffer-name) "*scratch*"))
            (register-val-jump-to content nil)
            (should (equal (hypb:buffer-file-name) file))
            (should (equal pos (point)))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-register-test--register-val-insert-ibut ()
  "Verify register val inserts ibut."
  (let ((file1 (make-temp-file "hypb"))
        (file2 (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (find-file file1)
          (insert "<[label]> ${HOME}")
          (goto-char 5)
          (let ((content (hui-register-struct-at-point)))
            (find-file file2)
	    ;; Inserts ebut into file2 that contains an ilink in file1
	    ;; that jumps to ${HOME}
            (register-val-insert content)
            (should (equal (hypb:buffer-file-name) file2))
            (goto-char 5)
            (should (ebut:at-p))
            (action-key)
            (should (equal (expand-file-name default-directory)
			   (file-name-as-directory (getenv "HOME"))))))
      (hy-delete-file-and-buffer file1)
      (hy-delete-file-and-buffer file2))))

(ert-deftest hui-register-test--register-val-insert-ebut ()
  "Verify register val inserts link to ebut."
  (let ((file1 (make-temp-file "hypb"))
        (file2 (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (find-file file1)
          (ebut:program "label" 'link-to-directory "/tmp")
          (goto-char 5)
          (let ((content (hui-register-struct-at-point)))
            (find-file file2)
            (register-val-insert content)
            (should (equal (hypb:buffer-file-name) file2))
            (goto-char 5)
            (should (ebut:at-p))
            (action-key)
            (should (equal major-mode 'dired-mode))
	    ;; Support c:/tmp on Windows too
            (should (member default-directory (list (expand-file-name "tmp/" "/")
						    "/private/tmp/")))))
      (hy-delete-file-and-buffer file1)
      (hy-delete-file-and-buffer file2))))

(provide 'hui-register-tests)
;;; hui-register-tests.el ends here
