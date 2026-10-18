;;; hypb-release-tests.el --- unit tests for hypb-release -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    16-Oct-26 at 18:32:18
;; Last-Mod:     16-Oct-26 at 19:33:26 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2026  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'el-mock)
(require 'hy-test-helpers)
(require 'hypb-release)

(ert-deftest hypb-release-tests--replace-version ()
  "Test `hypb-release--replace-version'."
  (with-temp-buffer
    (insert "Version: 1.0")
    (should-not (hypb-release--replace-version "Version: {version}" "1.1" "1.2" 'dry-run))
    (should (hypb-release--replace-version "Version: {version}" "1.0" "1.2" 'dry-run))
    (should (hypb-release--replace-version "Version: {version}" "1.0" "1.2"))
    (should (string= "Version: 1.2" (buffer-string)))))

(ert-deftest hypb-release-tests--update-version ()
  "Test `hypb-release-update-version'."
  (let* ((file1 (make-temp-file "hypb" nil nil "Version: 1.0"))
         (file2 (make-temp-file "hypb" nil nil "Version: 1.0 V1.0"))
         (hypb-release-version-file-list
          (list (list file1 "Version: {version}")
                (list file2 "Version: {version}")
                (list file2 "V{version}")
                (list "no-such-file" "Version: {version}" t))))
    (unwind-protect
        (mocklet (((lm-version "hyperbole.el") => "1.0"))
          (ert-with-message-capture cap
            (hypb-release-update-version "1.1")
            (hy-test-helpers:should-last-message
             "Version 1.0 -> 1.1: 3 replacements in 2 files" cap))
          (should (string-prefix-p "Version: 1.1" (hy-test-file-contents file1)))
          (should (string-prefix-p "Version: 1.1 V1.1" (hy-test-file-contents file2))))
      (hy-delete-files-and-buffers (list file1 file2)))))

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'hypb-release-tests)
;;; hypb-release-tests.el ends here
