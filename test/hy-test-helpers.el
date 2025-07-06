;;; hy-test-helpers.el --- unit test helpers         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:      6-Jul-25 at 15:40:23 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'hmouse-drv)                   ; For `action-key'
(require 'hywiki)                       ; For `hywiki-word-face-at-p'
(eval-when-compile (require 'cl-lib))

(defun hy-test-helpers:consume-input-events ()
  "Use `recursive-edit' to consume the events kbd-key generates."
  (run-with-timer 0.1 nil (lambda () (exit-recursive-edit)))
  (recursive-edit))

(defun hy-test-helpers:ensure-link-possible-type (type)
  "At point, ensure `hui:link-possible-types' returns a single TYPE."
  (let* ((possible-types (hui:link-possible-types))
	 (first-type (caar possible-types)))
    (should (= (length possible-types) 1))
    (should (equal first-type type))))

(defmacro hy-test-helpers:ert-simulate-keys (keys &rest body)
  "Execute BODY with KEYS as pseudo-interactive input.
Disable `vertico-mode' which can get in the way of standard key
processing."
  (declare (debug t) (indent 1))
  `(if (bound-and-true-p vertico-mode)
       (unwind-protect
	   (progn (vertico-mode 0)
		  (ert-simulate-keys ,keys ,@body))
	 (vertico-mode 1))
     (ert-simulate-keys ,keys ,@body)))

(defun hy-test-helpers:should-last-message (msg captured)
  "Verify MSG is in CAPTURED text."
  (should (string-search msg captured)))

(defun hy-test-helpers:action-key-should-call-hpath:find (str)
  "Call action-key and check that hpath:find was called with STR."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'hpath:find)
               (lambda (filename)
		 (if (not (and (stringp str) (stringp filename)))
		     (should (eq t (message "str = %s; filename = %s" str filename)))
		   (setq was-called (should (or (string= str filename)
						;; Support Windows paths
						(string= (expand-file-name str)
							 (expand-file-name filename)))))))))
      (action-key)
      (should was-called))))

(defun hy-test-helpers:hypb-function-should-call-hpath:find (function str)
  "Call FUNCTION and check that hpath:find was called with STR."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'hpath:find)
               (lambda (filename)
                 (setq was-called (should (or (string= str filename) (string= str (expand-file-name filename))))))))
      (funcall function)
      (should was-called))))

(defun hy-test-helpers:kill-buffer (buffer)
  "Kill BUFFER if it exists."
  (when (get-buffer buffer)
    (kill-buffer buffer)))

(cl-defun hy-test-helpers-verify-hattr-at-p (&key actype args loc lbl-key name)
  "Verify the attribute of hbut at point.
Checks ACTYPE, ARGS, LOC, LBL-KEY and NAME."
  (hbut:at-p)
  (should (eq (hattr:get 'hbut:current 'actype) actype))
  (should (equal (hattr:get 'hbut:current 'args) args))
  (should (equal (hattr:get 'hbut:current 'loc) loc))
  (should (equal (hattr:get 'hbut:current 'lbl-key) lbl-key))
  (should (equal (hattr:get 'hbut:current 'name) name)))

(defun hy-delete-file-and-buffer (file)
  "Delete FILE and buffer visiting file."
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (kill-buffer))))
  (delete-file file))

(defun hy-delete-files-and-buffers (files)
  "Delete all FILES and all buffers visiting those files."
  (dolist (f files)
    (hy-delete-file-and-buffer f)))

(defun hy-delete-dir-and-buffer (dir)
  "Delete DIR and buffer visiting directory."
  (let ((buf (find-buffer-visiting dir))
	(hywiki-cache (when (featurep 'hywiki)
			(expand-file-name hywiki-cache-default-file
					  dir))))
    (when buf
      (kill-buffer buf))
    (when (and hywiki-cache
	       (file-readable-p hywiki-cache)
	       (file-writable-p hywiki-cache))
      (delete-file hywiki-cache))
    (delete-directory dir)))

(defun hy-make-random-wikiword (&optional length word-length)
  "Create a random WikiWord (string).
The WikiWord will have a total of LENGTH characters.  Each word part of
the WikiWord will have WORD-LENGTH characters.  The default LENGTH is 12
and the default WORD-LENGTH is 4."
  (unless length
    (setq length 12))
  (unless word-length
    (setq word-length 4))
  (let ((chars (vconcat (number-sequence ?a ?z)))
        (result ""))
    (dotimes (i length)
      (when (zerop (mod i word-length))
        (setq result (concat result " ")))
      (setq result (concat result (char-to-string (elt chars (random (length chars)))))))
    (replace-regexp-in-string "[[:space:]]" "" (capitalize result))))

(defvar hy-test-run-failing-flag nil
  "Non-nil means test cases that are known to fail will be tried.")

(defun hy-test-word-face-at-region (beg end)
  "Non-nil if all chars in region [BEG, END] have `hywiki--word-face'."
  (interactive "r")
  (let (no-face)
    (while (and (< beg end) (not no-face))
      (unless (hywiki-word-face-at-p beg)
        (setq no-face t))
      (setq beg (1+ beg)))
    (not no-face)))

(provide 'hy-test-helpers)
;;; hy-test-helpers.el ends here
