;;; hywconfig-tests.el --- unit tests for hywconfig         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:      7-Dec-23 at 23:17:36 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2023  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Unit tests for "../hywconfig.el"

;;; Code:

(require 'hywconfig)
(require 'ert)

(defun hywconfig-tests--remove-ring ()
  "Remove ring from frame parameters."
  (set-frame-parameter nil 'hywconfig-ring nil))

(defun hywconfig-tests--remove-names ()
  "Remove names from frame parameters."
  (set-frame-parameter nil 'hywconfig-names nil))

(ert-deftest hywconfig--inital-ring-is-empty ()
  "Verify an initial ring is empty."
  (hywconfig-tests--remove-ring)
  (should (hywconfig-ring-empty-p)))

(ert-deftest hywconfig--not-empty-if-ring-save ()
  "Verify ring is not empty after saving a configuration."
  (hywconfig-tests--remove-ring)
  (hywconfig-ring-save)
  (should-not (hywconfig-ring-empty-p)))

(ert-deftest hywconfig--number-of-configs-are-store-up-to-a-max ()
  "Verify there is a max of number of configuration that can be saved."
  (let ((hywconfig-ring-max 2))
    (hywconfig-tests--remove-ring)
    (hywconfig-ring-save)
    (hywconfig-ring-save)
    (should (= (ring-length (hywconfig-get-ring)) hywconfig-ring-max))
    (hywconfig-ring-save)
    (should (= (ring-length (hywconfig-get-ring)) hywconfig-ring-max))))

(ert-deftest hywconfig--empty-after-pop-ring-with-one-config ()
  "Verify ring is not empty after saving a configuration."
  (hywconfig-tests--remove-ring)
  (hywconfig-ring-save)
  (hywconfig-delete-pop)
  (should (hywconfig-ring-empty-p)))

(ert-deftest hywconfig--max-minus-one-after-pop-ring-with-max-config ()
  "Verify ring is not empty after saving a configuration."
  (let ((hywconfig-ring-max 2))
    (hywconfig-tests--remove-ring)
    (hywconfig-ring-save)
    (hywconfig-ring-save)
    (hywconfig-delete-pop)
    (should (= (ring-length (hywconfig-get-ring)) (1- hywconfig-ring-max)))))

(ert-deftest hywconfig--get-not-existing-config-errors ()
  "Verify retrieving a config that has not been saved gives an error message."
  (hywconfig-tests--remove-names)
  (let ((err (should-error (hywconfig-restore-by-name "config") :type 'error)))
    (should (string-match-p "No window configuration for this frame named" (cadr err)))))

(ert-deftest hywconfig--add-by-name ()
  "Verify config is added by name."
  :expected-result :failed
  ;; See backtrace for the error -  (wrong-type-argument listp #<window-configuration>)
  ;; set:member("config" (#<window-configuration>))
  ;; set:get("config" (#<window-configuration>))
  ;; (let ((wconfig (set:get name (hywconfig-get-names)))) (if wconfig (progn (hywconfig-set-window-configuration wconfig) (if (called-interactively-p 'interactive) (progn (message "Window configuration `%s' is now active." name)))) (error "(hywconfig-restore-by-name): No window configuration for this frame named `%s'" name)))
  ;; (cond ((null name) (message "There is no named window configuration to restore.")) ((not (stringp name)) (error "(hywconfig-restore-by-name): `name' argument is not a string: %s" name)) (t (let ((wconfig (set:get name (hywconfig-get-names)))) (if wconfig (progn (hywconfig-set-window-configuration wconfig) (if (called-interactively-p 'interactive) (progn (message "Window configuration `%s' is now active." name)))) (error "(hywconfig-restore-by-name): No window configuration for this frame named `%s'" name)))))
  ;; hywconfig-restore-by-name("config")
  ;; [...]
  (hywconfig-tests--remove-names)
  (hywconfig-add-by-name "config")
  (should (hywconfig-restore-by-name "config")))

(ert-deftest hywconfig--delete-by-name ()
  "Verify config can be deleted by name."
  :expected-result :failed
  ;; Same error as above
  (hywconfig-tests--remove-names)
  (hywconfig-add-by-name "config")
  (hywconfig-delete-by-name "config")
  (let ((err (should-error (hywconfig-restore-by-name "config") :type 'error)))
    (should (string--p "No window configuration for this frame named" (cadr err)))))

(provide 'hywconfig-tests)
;;; hywconfig-tests.el ends here
