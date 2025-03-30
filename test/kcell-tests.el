;;; kcell-tests.el --- Test for kcells in Koutliner        -*- lexical-binding: t; -*-
;; usage:        GNU Emacs Lisp Library
;; keywords:     test
;;
;; author:       Mats Lidell
;; org:          Free Software Foundation, Inc.
;; e-mail:       matsl@gnu.org
;;
;; orig-date:    16-Feb-22 at 23:28:49
;; last-mod:     16-Mar-25 at 10:15:23 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2024  Free Software Foundation, Inc.
;; Licensed under the GNU General Public License, version 3.
;;
;; This file is not part of Emacs.  It requires Emacs 28 or above.
;; This file is part of Hyperbole.
;;
;;; Commentary:
;;
;; Tests for "../kotl/kcell.el"
;;

;;; Code:

(require 'kcell "kotl/kcell")
(require 'kotl-mode)
(require 'hy-test-helpers "test/hy-test-helpers")

(defconst kcell-tests--ref-to-id-tests
  ;  ref flag kvspec expected
  '((0 nil "ben" 0)
    (0 t "ben" 0)
    (1 nil "ben" 1)
    (1 t "ben" 1)

    ("1" nil "ben" 1)
    ("1" t "ben" 1)
    ("1a" nil "ben" 3)
    ("1a" t "ben" 3)
    ("1.1" nil "ben." 3)
    ("1.1" t "ben." 3)
    (01 nil "ben" 1)
    (01 t "ben" 1)

    ("1a=03" nil "ben" 3)
    ("1a=03" t "ben" 3)
    ("1.1=03" nil "ben." 3)
    ("1.1=03" t "ben." 3)

    ("1|xyz" nil "ben" 1)
    ("1|xyz" t "ben" "01|xyz")
    ("1a|xyz" nil "ben" 3)
    ("1a|xyz" t "ben" "03|xyz")
    ("1.1|xyz" nil "ben." 3)
    ("1.1|xyz" t "ben." "03|xyz")

    ("1a=03|xyz" nil "ben" 3)
    ("1a=03|xyz" t "ben" "03|xyz")
    ("1.1=03|xyz" nil "ben." 3)
    ("1.1=03|xyz" t "ben." "03|xyz")

    ("1=03|xyz" t "ben" "03|xyz")
    ("1.2=03|xyz" t "ben." "03|xyz")

    ("1a" nil "ben0" nil)
    ("1.1" nil "ben0" nil))
  "Test cases on the form list of REF FLAG KVSPEC EXPECTED.")

(defun kcell-tests--check-ref-to-id (spec)
  "Check if ref-to-id return expected value given in SPEC.
Return t if is does else return the SPEC."
  (let ((ref (nth 0 spec))
        (flag (nth 1 spec))
        (kvspec (nth 2 spec))
        (expected (nth 3 spec)))
    (kvspec:activate kvspec)
    (or (equal (kcell:ref-to-id ref flag) expected) spec)))

(ert-deftest kcell-tests--ref-to-id ()
  "Verify all ref to id transformations."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (kotl-mode:add-child)
          (let ((failures (delq t (mapcar (lambda (x) (funcall #'kcell-tests--check-ref-to-id x)) kcell-tests--ref-to-id-tests))))
            (if failures
                (ert-fail (cons "These refs were not correctly converted to ids:" failures))
              t)))
      (hy-delete-file-and-buffer kotl-file))))

(provide 'kcell-tests)
;;; kcell-tests.el ends here
