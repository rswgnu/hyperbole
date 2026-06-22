;;; hasht-tests.el --- unit tests for hasht -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    22-Jun-26 at 23:25:12
;; Last-Mod:     23-Jun-26 at 00:49:42 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2026  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'hasht)

(ert-deftest hasht-tests--make ()
  "Verify `hash-make'."
  (should (hash-empty-p (hash-make nil)))
  (let ((h1 (hash-make 1))
        (h10 (hash-make 10)))
    (should (hash-empty-p h1))
    (should (= 1 (hash-table-size h1)))
    (should (= 10 (hash-table-size h10))))
  (should (string= "val" (hash-lookup "key" (hash-make '(("val" . "key"))))))
  (should (string= "val2" (hash-lookup "key" (hash-make '(("val" . "key") ("val2" . "key")))))))

(ert-deftest hasht-tests--merge ()
  "Verify `hash-merge'."
  (let ((h1 (hash-make '(("val" . "key"))))
        (h2 (hash-make '(("val2" . "key2")))))
    (should (string= "val" (hash-lookup "key" h1)))
    (should (string= "val" (hash-lookup "key" (hash-merge h1))))
    (let ((m (hash-merge h1 h2)))
      (should (string= "val" (hash-lookup "key" m))))))

(provide 'hasht-tests)
;;; hasht-tests.el ends here
