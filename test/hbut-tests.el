;;; hbut-tests.el --- hbut unit tests         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-may-21 at 09:33:00
;; Last-Mod:      1-May-25 at 23:33:16 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; "../hbut.el"

;;; Code:

(require 'ert)
(require 'hbut)
(require 'hactypes)
(require 'hpath)
(require 'el-mock)
(require 'hy-test-helpers "test/hy-test-helpers")

(defun hbut-tests:should-match-tmp-folder (tmp)
  "Check that TMP matches either of \"/tmp/\" or \"/private/tmp/\".
Needed since hyperbole expands all links to absolute paths and
/tmp/ can be a symbolic link."
  (should (and (stringp tmp) (string-match-p "\"?\\(/\\|./\\|/private/\\)tmp/\"?\\'" tmp) t)))

(ert-deftest ebut-program-link-to-directory ()
  "Programatically create ebut with link-to-directory using `temporary-file-directory`."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory temporary-file-directory)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args (list temporary-file-directory) :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest ebut-program-link-to-directory-in-non-file-buffer ()
  "Programatically create ebut in non file buffer.
Create button with link-to-directory using `temporary-file-directory`."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory temporary-file-directory)
    (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args (list temporary-file-directory) :loc (current-buffer) :lbl-key "label")))

(ert-deftest ebut-program-link-to-directory-in-message-mode-buffer ()
  "Programatically create ebut in message mode buffer.
Create button with link-to-directory using `temporary-file-directory`."
  (with-temp-buffer
    (unwind-protect
        (progn
          (message-mode)
          (ebut:program "label" 'link-to-directory temporary-file-directory)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args (list temporary-file-directory) :loc (current-buffer) :lbl-key "label"))
      (set-buffer-modified-p nil))))

(ert-deftest ebut-program-shell-cmd ()
  "Programatically create ebut running shell command."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'exec-shell-cmd "ls /tmp")
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::exec-shell-cmd :args '("ls /tmp") :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest ebut-delete-removes-ebut ()
  "Remove an ebut."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory "/tmp/")
          (should (hbut:at-p))
          (should (hui:hbut-delete))
          (should-not (hbut:at-p)))
      (hy-delete-file-and-buffer file))))

(ert-deftest ebut-delete-removes-ebut-in-non-file-buffer ()
  "Remove an ebut from non file buffer."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp/")
    (should (hbut:at-p))
    (should (hui:hbut-delete))
    (should-not (hbut:at-p))))

(ert-deftest ebut-delete-removes-ebut-in-message-mode-buffer ()
  "Remove an ebut from `message-mode' buffer."
  (with-temp-buffer
    (message-mode)
    (ebut:program "label" 'link-to-directory "/tmp/")
    (should (hbut:at-p))
    (should (hui:hbut-delete))
    (should-not (hbut:at-p))))

(ert-deftest gbut-program-calls-ebut-program ()
  "Programatically create gbut calls ebut:program."
  (let ((test-file (make-temp-file "test-file")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(with-mock
          (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
          (mock (ebut:program "label" 'link-to-directory "/tmp/") => t)
          (gbut:ebut-program "label" 'link-to-directory "/tmp/"))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest gbut-program-link-to-directory ()
  "Programatically create gbut with link-to-directory."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-directory "/tmp/"))
	  (with-current-buffer test-buffer
            (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
            (hbut-tests:should-match-tmp-folder (car (hattr:get (hbut:at-p) 'args)))
            (should (equal (hattr:get (hbut:at-p) 'loc) test-file))
            (should (equal (hattr:get (hbut:at-p) 'lbl-key) "global"))))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest gbut-program-eval-elisp ()
  "Programatically create gbut with eval-elisp."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'eval-elisp '()))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::eval-elisp :args  '(()) :loc test-file :lbl-key "global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest gbut-program-link-to-file ()
  "Programatically create gbut with eval-elisp."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-file test-file))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file :args (list test-file) :loc test-file :lbl-key "global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest gbut-program-link-to-file-line ()
  "Programatically create gbut with eval-elisp."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-file-line test-file 10))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line :args (list test-file 10) :loc test-file :lbl-key "global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest gbut-program-link-to-file-line-and-column ()
  "Programatically create gbut with eval-elisp."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-file-line-and-column test-file 10 20))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line-and-column
					       :args (list test-file 10 20) :loc test-file :lbl-key "global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest hypb:program-create-ebut-in-buffer ()
  "Create button with hypb:program in buffer."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp/")
    (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
    (hbut-tests:should-match-tmp-folder (car (hattr:get (hbut:at-p) 'args)))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))))

(ert-deftest hypb:program-create-ebut-in-buffer-with-same-label ()
  "Create button with same label shall add number so it is unique."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp/")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))
    (goto-char (point-max))
    (ebut:program "label" 'link-to-directory "/tmp/")
    (goto-char (- (point-max) 2))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label:2"))))

(ert-deftest hypb:program-create-link-to-file-line-and-column-but-in-file ()
  "Create button that links to file with line and column with hypb:program in buffer."
  (let ((test-file (make-temp-file "test-file")))
    (unwind-protect
        (progn
          (find-file test-file)
          (ebut:program "label" 'link-to-file-line-and-column test-file 2 3)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line-and-column :args (list test-file 2 3) :loc test-file :lbl-key "label"))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest hypb--ebut-at-p-should-not-insert-hbdata-section-in-non-file-buffers ()
  "Verify that ebut:at-p does not insert a hbdata section in a non file buffer."
  (with-temp-buffer
    (let ((button "<(unknown)>"))
      (insert button)
      (goto-char 3)
      (should-not (ebut:at-p))
      (should (string= button (buffer-string))))))

(ert-deftest hbut-tests-ibut-program-link-to-directory ()
  "Programmatically create ibut link-to-directory."
  (with-temp-buffer
    (ibut:program "name" 'link-to-directory "/tmp/")
    (should (string= "<[name]> - \"/tmp/\"" (buffer-string)))))

(ert-deftest hbut-tests-ibut-program-link-to-file ()
  "Programatically create ibut link to file."
  (let ((test-file (make-temp-file "ibut" nil ".txt"))
        (default-directory "/home/user/directory/")) ; Make link be absolute
    (unwind-protect
        (with-temp-buffer
          (ibut:program "name" 'link-to-file test-file)
          (should (string=
                   (concat "<[name]> - \"" test-file "\"")
                   (buffer-string))))
      (delete-file test-file))))

(ert-deftest hbut-tests-ibut-insert-text-link-to-dir ()
  "Insert link to dir."
  (with-temp-buffer
    (ibut:program "name" 'link-to-directory "/tmp/")
    (should (string= "<[name]> - \"/tmp/\"" (buffer-string)))
    (goto-char 3)
    (let ((but (ibut:at-p))
          (default-directory "/home/user/directory/")) ; Make link be absolute
      (should but)
      (with-temp-buffer
        (ibut:insert-text but)
	;; Allow for /tmp/ being a link to /private/tmp/ on Macos
        (should (string-match "\"\\(/private\\)?/tmp/\"" (buffer-string)))))))

(ert-deftest hbut-tests-ibut-insert-annot-bib ()
  "Insert ibut to annot-bib, which must be attached to a file."
  (let ((annot-bib-file (make-temp-file "annot-bib" nil ".txt"))
	annot-bib-buf)
    (unwind-protect
        (progn
	  ;; Test with name
          (setq annot-bib-buf (find-file annot-bib-file))
	  (ibut:program "name" 'annot-bib "arg")
	  (save-buffer)
	  (should (string-match (concat (regexp-quote "<[name]> - [arg]")
					"\\s-*")
				(buffer-string)))
	  ;; Test without name
	  (erase-buffer)
	  (ibut:program nil 'annot-bib "arg")
	  (save-buffer)
	  (should (string-match (concat (regexp-quote "[arg]")
					"\\s-*")
				(buffer-string))))
      (kill-buffer annot-bib-buf)
      (hy-test-helpers:kill-buffer annot-bib-file))))

(ert-deftest hbut-tests-ibut-insert-kbd-key ()
  "Insert ibut to kbd-key."
  (let ((kbd-key-file (make-temp-file "kbd-key" nil ".txt"))
	kbd-key-buf)
    (unwind-protect
        (progn
	  ;; Test with name
          (setq kbd-key-buf (find-file kbd-key-file))
	  (ibut:program "name" 'kbd-key "{ C-f C-f }")
	  (save-buffer)
	  (should (string-match (concat (regexp-quote "<[name]> - { C-f C-f }")
					"\\s-*")
				(buffer-string)))
	  ;; Test without name
	  (erase-buffer)
	  (ibut:program nil 'kbd-key "{ C-f C-f }")
	  (save-buffer)
	  (should (string-match (concat (regexp-quote "{ C-f C-f }")
					"\\s-*")
				(buffer-string))))
      (kill-buffer kbd-key-buf)
      (hy-test-helpers:kill-buffer kbd-key-file))))

(defconst hbut-tests-actypes-list
  '(
    ;; Would have to create a file with a valid Org ID in it to test this:
    ;; (actypes::link-to-org-id "id:arg1")
    ;;
    ;; Use the DEMO file to test this, as it requires a file
    ;; (actypes::annotate-bib "[FSF 12]" nil)
    ;;
    (ibtypes::klink actypes::link-to-kotl "<~/EXAMPLE.kotl, 3b=06>" "<~/EXAMPLE.kotl, 3b=06>")
    (ibtypes::man-apropos man "rm(1) - remove" "rm(1) - remove")
    (ibtypes::pathname actypes::exec-shell-cmd "\"!/bin/bash\"" "/bin/bash")
    (ibtypes::pathname actypes::exec-window-cmd "\"&/bin/bash\"" "/bin/bash")
    (ibtypes::kbd-key actypes::kbd-key "{C-h h}" "C-h h")
    (ibtypes::glink actypes::link-to-gbut "<glink:arg1>" "arg1")
    (ibtypes::elink actypes::link-to-ebut "<elink:arg1>" "arg1")
    (ibtypes::elink actypes::link-to-ebut "<elink:arg1: arg2>" "arg1" "arg2")
    (ibtypes::ilink actypes::link-to-ibut "<ilink:arg1>" "arg1")
    (ibtypes::ilink actypes::link-to-ibut "<ilink:arg1: arg2>" "arg1" "arg2")
    (ibtypes::pathname actypes::link-to-file "\"/etc/passwd\"" "/etc/passwd")
    (ibtypes::pathname-line-and-column actypes::link-to-file-line "\"/etc/passwd:10\"" "/etc/passwd" 10)
    (ibtypes::rfc actypes::link-to-rfc "rfc123" 123))
  "hbut actypes test list is in the format:
     (implicit-button-type action-type expected-implicit-button-text &rest implicit-button-args)")

(ert-deftest hbut-tests-ibut-insert-links ()
  "Test that `ibut:program' correctly inserts many types of link ibuttons."

  ;; Needed for actypes::link-to-kotl test below
  (save-window-excursion
    (kotl-mode:example)
    (bury-buffer))

  (let ((name)
	(body '(let ((expected-ibut-string))
		 (apply #'ibut:program name (nth 1 bd) (cdddr bd))
		 (goto-char 3)
		 (setq expected-ibut-string (nth 2 bd))
		 (when name
		   (setq expected-ibut-string (format "<[%s]> - %s" name expected-ibut-string)))
		 (should (string-equal expected-ibut-string (buffer-string)))
		 (should (ibut:at-p))
		 (let ((ibtype (hattr:get 'hbut:current 'categ))
		       (actype (hattr:get 'hbut:current 'actype))
		       (expected-ibtype (nth 0 bd))
		       (expected-actype (nth 1 bd))
		       (args (hattr:get 'hbut:current 'args))
		       (hrule:action #'actype:identity))
		   ;; Certain actypes call hact with a different actype within
		   ;; their bodies; this captures the final actype executed,
		   ;; which is the one we compare against.
		   (when (memq actype '(actypes::link-to-file klink:act))
		     (save-excursion
		       (apply #'actype:act actype args)
		       (setq actype (hattr:get 'hbut:current 'actype))))
		   ;; These should show the value of these variables when you
		   ;; use the {l} command on failure, allowing quick debugging.
		   (should expected-ibtype)
		   (should expected-actype)
		   (should actype)
		   (should (or args t))
		   (should (eq ibtype expected-ibtype)
			   (should (or (eq actype expected-actype)
				       (eq (actype:elisp-symbol actype) expected-actype))))))))

    `(dolist (bd ,hbut-tests-actypes-list)
       (with-temp-buffer ,@body))

    (setq name "name")
    `(dolist (bd ,hbut-tests-actypes-list)
       (with-temp-file "hypb.txt" ,@body))))

;; ibut:operate tests

(ert-deftest hbut-tests--ibut-operate--none ()
  "Test creation of an unnamed ibut.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  1 | nil  | nil      | nil    | nil  | create: unnamed ibut from hbut:current attrs  |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let (buf-str)
      (insert "/tmp/")
      (goto-char 2)
      (should (hbut:at-p))
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))
      ;; Test that ibut:operate produces an error and leaves in-buffer button unchanged
      (erase-buffer)
      (ibut:operate)
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--fail-create-add-name ()
  "Test that trying to add `new-name' to an unnamed ibutton fails when `edit-flag' is nil.
See #10 for the proper way to add an ibutton name.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  2 | nil  | new-name | nil    | nil  | ERROR: edit-flag must be t to set new-name    |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let (buf-str)
      (insert "/tmp/")
      (goto-char 2)
      (should (hbut:at-p))
      ;; Test that ibut:operate errors and leaves in-buffer button unchanged
      (goto-char (point-min))
      (should-error (ibut:operate "new-name"))
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (hbut-tests:should-match-tmp-folder buf-str))))

(ert-deftest hbut-tests--ibut-operate--name ()
  "Test that ibut get created with `name' from button attributes.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  3 | name | nil      | nil    | nil  | create: ibut with name                        |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "<[name]> - /tmp/")
	  buf-str)
      (insert ibut-str)
      (goto-char 2)
      (should (hbut:at-p))
      (should (equal "name" (hattr:get 'hbut:current 'name)))
      ;; Test that ibut:operate properly creates an in-buffer ibut from its in-memory form
      (erase-buffer)
      (ibut:operate)
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (hbut-tests:should-match-tmp-folder buf-str)
      (should (equal "name" (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ibut-operate--fail-rename-from-name ()
  "Test that named ibut rename to `new-name' fails when `edit-flag' is nil.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  4 | name | new-name | nil    | nil  | ERROR: create can't have name and new-name    |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "<[name]> - /tmp/")
	  buf-str)
      (insert ibut-str)
      (goto-char 2)
      (should (hbut:at-p))
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))
      ;; Test that ibut:operate produces an error and leaves in-buffer button unchanged
      (should-error (ibut:operate "new-name"))
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (hbut-tests:should-match-tmp-folder buf-str))))

(ert-deftest hbut-tests--ibut-operate--fail-rename-from-name-ignore-region ()
  "Test that named ibut rename to `new-name' fails; region active, `edit-flag' nil.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  5 | name | new-name | region | nil  | ERROR: create can't have name and new-name    |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "<[name]> - \"/tmp/\"")
	  buf-str)
      (insert ibut-str)
      (goto-char 2)
      (should (hbut:at-p))
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))
      ;; Test that ibut:operate produces an error and leaves in-buffer button unchanged
      (goto-char (point-min))
      (insert "words in buffer ")
      (mark-whole-buffer)
      (should-error (ibut:operate "new-name"))
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (goto-char (- (point-max) 2))
      (hbut-tests:should-match-tmp-folder buf-str))))

(ert-deftest hbut-tests--ibut-operate--name-ignore-region ()
  "Test creation of a named ibut and ignore region.
   |----q+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  6 | name | nil      | region | nil  | create: ibut with name (ignore region)        |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "<[name]> - /tmp/")
	  buf-str)
      (insert ibut-str "\nabcd")
      (mark-whole-buffer)
      (goto-char 2)
      (should (hbut:at-p))
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      ;; Test that ibut:operate properly creates an in-buffer ibut from its in-memory form
      (erase-buffer)
      (ibut:operate)
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--region ()
  "Test creation of a new ibut named from active region.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  7 | nil  | nil      | region | nil  | create: region named ibut                     |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "region - /tmp/")
	  buf-str)
      (insert ibut-str)
      (goto-char (point-min))
      (mark-word)
      ;; Test that ibut:operate properly creates an in-buffer ibut from its in-memory form
      (ibut:operate)
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (goto-char 2)
      (hbut-tests:should-match-tmp-folder buf-str)
      (should (equal "region" (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ibut-operate--fail-new-name-ignore-region ()
  "Test creation of a named ibut and ignore region.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  8 | nil  | new-name | region | nil  | ERROR: edit-flag must be t to set new-name    |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "<[abcd]> - \"/tmp/\"")
	  buf-str)
      (insert ibut-str)
      (mark-whole-buffer)
      (goto-char (- (point-max) 2))
      (should (hbut:at-p))
      (should (region-active-p))
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      ;; Test that ibut:operate properly creates an in-buffer ibut from its in-memory form
      (should-error (ibut:operate "new-name"))
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--remove-name ()
  "Test removal of any name from ibut at point.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   |  9 | nil  | nil      | nil    | t    | mod: remove any name from ibut                |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "<[name]> - \"/tmp/\"")
	  buf-str)
      (insert ibut-str)
      (goto-char 2)
      (should (hbut:at-p))
      (hattr:set 'hbut:current 'name nil)
      (ibut:operate nil t)
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (goto-char 2)
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))
      (should (null (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ibut-operate--add-new-name ()
  "Test addition of `new-name' to ibut at point.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   | 10 | nil  | new-name | nil    | t    | mod: add new-name as ibut's name attribute    |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "/tmp/")
	  buf-str)
      (insert ibut-str)
      (goto-char 2)
      (should (hbut:at-p))
      ;; Test that ibut:operate properly creates an in-buffer ibut from its in-memory form
      (ibut:operate "new-name" t)
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (goto-char 2)
      (should (hbut:at-p))
      (should (equal "new-name" (hattr:get 'hbut:current 'name)))
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--add-name ()
  "Test addition of `name' to ibut at point and any in-buffer copies.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   | 11 | name | nil      | nil    | t    | mod: name of ibut from hbut:current attrs     |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "\"/tmp/\"")
	  buf-str)
      (insert ibut-str)
      (goto-char 2)
      (should (hbut:at-p))
      ;; Test that ibut:operate properly creates an in-buffer ibut from its in-memory form
      (hattr:set 'hbut:current 'name "name")
      (ibut:operate nil t)
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (goto-char 2)
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))
      (should (equal "name" (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ibut-operate--rename-from-name ()
  "Test that named ibut rename to `new-name' fails when `edit-flag' is nil.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   | 12 | name | new-name | nil    | t    | mod: rename ibut with name to new-name        |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    ;; Create in-buffer and in-memory ibut
    (let ((ibut-str "<[name]> - \"/tmp/\"")
	  buf-str)
      (insert ibut-str)
      (goto-char 2)
      (should (hbut:at-p))
      ;; Test that ibut:operate properly creates an in-buffer ibut from its in-memory form
      (ibut:operate "new-name" t)
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (goto-char 2)
      (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
      (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))
      (should (equal "new-name" (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ibut-operate--fail-rename-ignore-region ()
  "Test modification failure of an named ibut with `new-name' and active region.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   | 13 | name | new-name | region | t    | ERROR: Can't use region to mod existing ibut  |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    (let (buf-str)
      (insert "<[name]> - /tmp/")
      (goto-char 2)
      (should (hbut:at-p))
      (set-mark (point-max))
      (should (region-active-p))
      (should-error (ibut:operate "new-name" t))
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (hbut-tests:should-match-tmp-folder buf-str)
      (should (equal "name" (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ibut-operate--fail-name-ignore-region ()
  "Test modification failure of a named ibut with active region.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   | 14 | name | nil      | region | t    | ERROR: Can't use region to mod existing ibut  |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    (let (buf-str)
      (insert "<[name]> - /tmp/")
      (goto-char 2)
      (should (hbut:at-p))
      (set-mark (point-max))
      (should (region-active-p))
      (should-error (ibut:operate nil t))
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (hbut-tests:should-match-tmp-folder buf-str)
      (should (equal "name" (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ibut-operate--fail-rename-from-region ()
  "Test modification failure of an unnamed ibut with active region.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   | 15 | nil  | nil      | region | t    | ERROR: Can't use region to mod existing ibut  |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    (let (buf-str)
      (insert "/tmp/")
      (goto-char 2)
      (should (hbut:at-p))
      (set-mark (point-max))
      (should (region-active-p))
      (should-error (ibut:operate nil t))
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (hbut-tests:should-match-tmp-folder buf-str)
      (should (null (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ibut-operate--fail-add-new-name-ignore-region ()
  "Test modification failure of an unnamed ibut with `new-name' and active region.
   |----+------+----------+--------+------+-----------------------------------------------|
   |  # | name | new-name | region | edit | operation                                     |
   |----+------+----------+--------+------+-----------------------------------------------|
   | 16 | nil  | new-name | region | t    | ERROR: Can't use region to mod existing ibut  |
   |----+------+----------+--------+------+-----------------------------------------------|"
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    (let (buf-str)
      (insert "/tmp/")
      (goto-char 2)
      (should (hbut:at-p))
      (set-mark (point-max))
      (should (region-active-p))
      (should-error (ibut:operate "new-name" t))
      (setq buf-str (buffer-substring-no-properties (point-min) (point-max)))
      (message buf-str)
      (hbut-tests:should-match-tmp-folder buf-str)
      (should (null (hattr:get 'hbut:current 'name))))))

(ert-deftest hbut-tests--ebut-act-calls-hbut-act ()
  "Verify `ebut:act' calls `hbut:act'."
  (mocklet (((hbut:act 'button) => t)
            ((ebut:is-p 'button) => t))
    (should (ebut:act 'button)))
  (mocklet (((hbut:act 'hbut:current) => t)
            ((ebut:is-p 'hbut:current) => t))
    (should (ebut:act)))
  (mocklet ((ebut:is-p => nil))
    (should-error (ebut:act 'button))
    (should-error (ebut:act)))
  (progn
    (hattr:clear 'hbut:current)
    (should-error (ebut:act))))

(ert-deftest hbut-tests--ibut-act-calls-hbut-act ()
  "Verify `ibut:act' calls `hbut:act'."
  (mocklet (((hbut:act 'button) => t)
            ((ibut:is-p 'button) => t))
    (should (ibut:act 'button)))
  (mocklet (((hbut:act 'hbut:current) => t)
            ((ibut:is-p 'hbut:current) => t))
    (should (ibut:act)))
  (mocklet ((ibut:is-p => nil))
    (should-error (ibut:act 'button))
    (should-error (ibut:act)))
  (progn
    (hattr:clear 'hbut:current)
    (should-error (ibut:act))))

(ert-deftest hbut-tests--ibut-at-p-identifies-a-remote-pathname ()
  "Verify `ibut:at-p' identifies a remote pathname."
  (hattr:clear 'hbut:current)
  (with-temp-buffer
    (insert "/ssh:user@host.org:/home/user/file\n")
    (goto-char 4)
    (should (equal (hattr:get (ibut:at-p) 'categ) 'ibtypes::pathname))
    (goto-char (- (point-max) 5))
    (should (equal (hattr:get (ibut:at-p) 'categ) 'ibtypes::pathname))
    (goto-char 8)  ;; ibtypes::mail-address !!
    (should (equal (hattr:get (ibut:at-p) 'categ) 'ibtypes::pathname))))

(ert-deftest hypb--gbut-act-with-web-link ()
  "Verify `gbut:act' with a web link calls browser."
  (defvar global-but-file)
  (let ((global-but-file (make-temp-file "gbut" nil ".txt"
                                         "<[Link]> - \"https://savannah.gnu.org/projects/hyperbole/\"\n")))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file)
                  ((browse-url "https://savannah.gnu.org/projects/hyperbole/") => t)
                  (hpath:find-noselect => (find-file-noselect global-but-file)))
          (gbut:act "Link"))
      (hy-delete-file-and-buffer global-but-file))))

(ert-deftest hbut-tests--hattr-is-p ()
  "Verify `hattr:is-p'."
  (should-not (hattr:is-p "non symbol" 'any))
  (should-not (hattr:is-p nil 'any))
  (mocklet (((hattr:get 'hbut:current 'symbol) => 'value))
    (should (hattr:is-p 'symbol 'value))
    (should-not (hattr:is-p 'symbol 'other-value)))
  (mocklet (((hattr:get 'hbut-symbol 'symbol) => 'value))
    (should (hattr:is-p 'symbol 'value 'hbut-symbol))
    (should-not (hattr:is-p 'symbol 'other-value 'hbut-symbol))))

(ert-deftest hbut-tests--hattr-ibtype-is-p ()
  "Verify `hattr:ibtype-is-p'."
  (mocklet (((hattr:is-p 'categ 'ibtype nil) => t))
    (should (hattr:ibtype-is-p 'ibtype)))
  (mocklet (((hattr:is-p 'categ 'ibtype 'ibut-symbol) => t))
    (should (hattr:ibtype-is-p 'ibtype 'ibut-symbol))))

(ert-deftest hbut-tests--hattr-actype-is-p ()
  "Verify `hattr:actype-is-p'."
  (mocklet (((hattr:is-p 'actype 'ibtype nil) => t))
    (should (hattr:actype-is-p 'ibtype)))
  (mocklet (((hattr:is-p 'actype 'ibtype 'ibut-symbol) => t))
    (should (hattr:actype-is-p 'ibtype 'ibut-symbol))))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hbut-tests)
;;; hbut-tests.el ends here
