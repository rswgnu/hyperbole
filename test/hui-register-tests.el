;;; hui-register-tests.el --- test for hui-register         -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    10-Sep-22 at 20:43:17
;; Last-Mod:     14-Sep-22 at 22:24:50 by Mats Lidell
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

(ert-deftest hui-register-test--create-register-content ()
  "Verify the struct contains its parts."
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (find-file file)
        (insert "<[label label]> $HOME")
        (goto-char 5)
        (let ((content (hui-register-struct-at-point)))
          (should (equal (hui-register-but-label content) "label_label"))
          (should (equal (hui-register-but-link content) 'link-to-ibut))
          (should (markerp (hui-register-but-mpos content)))
          (should (equal (marker-buffer (hui-register-but-mpos content)) (current-buffer)))
          (should (equal (hui-register-but-file content) (buffer-file-name)))))
    (delete-file file)))

(ert-deftest hui-register-test--register-val-jump-to ()
  "Verify register val jumps to right file."
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (find-file file)
        (insert "<[label]> $HOME")
        (goto-char 5)
        (let ((content (hui-register-struct-at-point))
              (pos (point)))
          (set-buffer "*scratch*")
          (should (equal (buffer-name) "*scratch*"))
          (register-val-jump-to content nil)
          (should (equal (buffer-file-name) file))
          (should (equal pos (point)))))
    (delete-file file)))

(ert-deftest hui-register-test--register-val-insert ()
  "Verify register val inserts link to button."
  (let ((file1 (make-temp-file "hypb"))
        (file2 (make-temp-file "hypb")))
    (unwind-protect
        (find-file file1)
        (insert "<[label]> $HOME")
        (goto-char 5)
        (let ((content (hui-register-struct-at-point))
              (pos (point)))
          (find-file file2)
          (register-val-insert content)
          (should (equal (buffer-file-name) file2))
          (goto-char 5)
          (should (ebut:at-p))
          (action-key)
          (should (equal (buffer-file-name) file1))))
    (delete-file file1)
    (delete-file file2)))

(provide 'hui-register-tests)
;;; hui-register-tests.el ends here
