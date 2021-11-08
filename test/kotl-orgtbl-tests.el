;;; kotl-orgtbl-tests.el --- kotl orgtbl tests            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mats Lidell

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 2-Nov-21 at 17:04:30
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for kotl-orgtbl in "../kotl/kotl-orgtbl.el"

;;; Code:

(require 'ert)
(require 'kotl-mode "kotl/kotl-mode")

(load (expand-file-name "hy-test-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))
(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")

(ert-deftest kotl-orgtbl-enabled-uses-kotl-mode-delete-char-ouside-of-table ()
  "kotl-mode:detele-char is used ouside of org table."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")

          ;; Create an org table and leave point at end of cell
          (should (hact 'kbd-key "RET |-| RET"))
          (hy-test-helpers:consume-input-events)

          ;; Verify that kotl-mode:delete-char is used outside of the
          ;; table
          (condition-case err
              (progn
                (should (hact 'kbd-key "C-d"))
                (hy-test-helpers:consume-input-events))
            (error
             (progn
               (should (equal (car err) 'error))
               (should (string-match "(kotl-mode:delete-char): End of cell" (cadr err)))))
            (:success (ert-fail "C-d shall fail when deleting at the end of a cell."))))
      (delete-file kotl-file))))

(provide 'kotl-orgtbl-tests)
;;; kotl-orgtbl-tests.el ends here
