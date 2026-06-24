;;; hasht-tests.el --- unit tests for hasht -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    22-Jun-26 at 23:25:12
;; Last-Mod:     24-Jun-26 at 12:26:55 by Bob Weiner
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
(require 'ert-x)
(require 'hy-test-helpers)
(require 'hasht)
(require 'set)

(ert-deftest hasht-tests--make ()
  "Verify `hash-make'."
  (should (hash-empty-p (hash-make nil)))
  (let ((h1 (hash-make 1))
        (h10 (hash-make 10)))
    (should (hash-empty-p h1))
    (should (= 1 (hash-table-size h1)))
    (should (= 10 (hash-table-size h10))))
  (should (= 1 (hash-lookup "k" (hash-make '((1 . "k"))))))
  (should (= 2 (hash-lookup "k" (hash-make '((1 . "k") (2 . "k"))))))
  ;; Reverse
  (should (= 2 (hash-lookup "k" (hash-make '(("k" . 1) ("k" . 2)) t)))))

(ert-deftest hasht-tests--make-prepend ()
  "Verify `hash-make-prepend'."
  (should (equal '(1) (hash-lookup "k1" (hash-make-prepend '((1 . "k1"))))))
  (should (equal '(2 1) (hash-lookup "k1" (hash-make-prepend '((1 . "k1") (2 . "k1"))))))
  ;; Reverse
  (should (equal '(1) (hash-lookup "k1" (hash-make-prepend '(("k1" . 1)) t)))))

(ert-deftest hasht-tests--add ()
  "Verify `hash-add'."
  (let ((h (hash-make nil)))
    ;; Add one
    (should (= 1 (hash-add 1 "k" h)))
    (should (= 1 (hash-count h)))
    ;; Add with same key should replace
    (should (= 2 (hash-add 2 "k" h)))
    (should (= 1 (hash-count h)))
    ;; And one more
    (should (= 2 (hash-add 2 "k2" h)))
    (should (= 2 (hash-count h)))
    ;; Add value nil for key removes it
    (should-not (hash-add nil "k" h))
    (should-not (hash-key-p "k" h))))

(ert-deftest hasht-tests--map ()
  "Verify `hash-map´."
  (let ((h (hash-make '((1 . "k1") (2 . "k2")))))
    (should (set:equal '((1 . "k1") (2 . "k2")) (hash-map #'identity h)))
    (should (set:equal '(2 1) (hash-map #'car h)))
    (should (set:equal '("k2" "k1") (hash-map #'cdr h)))))

(ert-deftest hasht-tests--copy ()
  "Verify `hash-copy'."
  (let* ((h (hash-make '((1 . "k"))))
         (copy (hash-copy h)))
    (should (= 1 (hash-lookup "k" h)))
    (should (= 1 (hash-lookup "k" copy)))))

(ert-deftest hasht-tests--deep-copy ()
  "Verify `hash-deep-copy'."
  (let* ((h (hash-make '(((1  2) . "k"))))
         (copy (hash-deep-copy h)))
    (should (equal '(1 2) (hash-lookup "k" h)))
    (should (equal '(1 2) (hash-lookup "k" copy)))
    (should-not (eq (hash-lookup "k" h) (hash-lookup "k" copy)))))

(ert-deftest hasht-tests--delete ()
  "Verify `hash-delete'."
  (let ((h (hash-make '((1 . "k1")))))
    (should (= 1 (hash-count h)))
    (should (hash-delete "k1" h))
    (should (= 0 (hash-count h)))
    (should-not (hash-delete "k1" h))
    (should-not (hash-delete "k_other" h))))

(ert-deftest hasht-tests--key-p ()
  "Verify `hash-key-p'."
  (let ((h (hash-make '((1 . "k")))))
    (should (hash-key-p "k" h))
    (should (= 1 (hash-lookup "k" h)))
    (should-not (hash-key-p "1" h))))

(ert-deftest hasht-tests--merge ()
  "Verify `hash-merge'."
  (let ((h (hash-make '((1 . "k")))))
    (should (= 1 (hash-lookup "k" (hash-merge h))))
    (should (equal '(1 1) (hash-lookup "k" (hash-merge h h))))))

(ert-deftest hasht-tests--merge-list-val ()
  "Verify `hash-merge' with list value."
  (let ((h (hash-make '(((1 2) . "k")))))
    (should (equal '(1 2) (hash-lookup "k" (hash-merge h h))))))

(ert-deftest hasht-tests--prepend ()
  "Verify `hash-prepend'."
  (let ((h (hash-make '((1 . "k")))))
    (should-error (hash-prepend 2 "k" h))
    (should (hash-add '(1) "k" h))
    (should (equal '(2 1) (hash-prepend 2 "k" h)))))

(ert-deftest hasht-tests--prin1 ()
  "Verify `hash-prin1'."
  (ert-with-message-capture cap
    (hash-prin1 (hash-make '((1 . "k"))))
    (hy-test-helpers:should-last-message "(\n(1 . \"k\")\n)\n" cap)))

(ert-deftest hasht-tests--replace ()
  "Verify `hash-replace'."
  (let ((h (hash-make '((1 . "k")))))
    (should (= 2 (hash-replace 2 "k" h)))
    (should (= 2 (hash-lookup "k" h)))))

(ert-deftest hasht-tests--size ()
  "Verify `hash-size'."
  (should (<= 1 (hash-size (hash-make 1))))
  (should (<= 10 (hash-size (hash-make 10)))))

(provide 'hasht-tests)
;;; hasht-tests.el ends here
