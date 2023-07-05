;;; hbut-tests.el --- hbut unit tests         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-may-21 at 09:33:00
;; Last-Mod:      5-Jul-23 at 00:29:02 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
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
  "Check that TMP matches either of \"/tmp\" or \"/private/tmp\".
Needed since hyperbole expands all links to absolute paths and
/tmp can be a symbolic link."
  (should (and (stringp tmp) (string-match-p "\\`\"?\\(/\\|./\\|/private/\\)tmp\"?\\'" tmp) t)))

(ert-deftest ebut-program-link-to-directory ()
  "Programatically create ebut with link-to-directory."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory "/tmp")
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
          (hbut-tests:should-match-tmp-folder (car (hattr:get (hbut:at-p) 'args)))
          (should (equal (hattr:get (hbut:at-p) 'loc) file))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (hy-delete-file-and-buffer file))))

(ert-deftest ebut-program-link-to-directory-2 ()
  "Programatically create ebut with link-to-directory using `temporary-file-directory`."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory temporary-file-directory)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args (list temporary-file-directory) :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest ebut-program-shell-cmd ()
  "Programatically create ebut running shell command."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'exec-shell-cmd "ls /tmp")
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::exec-shell-cmd :args '("ls /tmp") :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest ebut-delete-removes-ebut-and-returns-button-data ()
  "Remove an ebut."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory "/tmp")
          (should (hbut:at-p))
          (let ((success-flag (hui:hbut-delete)))
            (should-not (hbut:at-p))
            (should (eq success-flag t))))
      (hy-delete-file-and-buffer file))))

(ert-deftest gbut-program-calls-ebut-program ()
  "Programatically create gbut calls ebut:program."
  (let ((test-file (make-temp-file "test-file")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(with-mock
          (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
          (mock (ebut:program "label" 'link-to-directory "/tmp") => t)
          (gbut:ebut-program "label" 'link-to-directory "/tmp"))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest gbut-program-link-to-directory ()
  "Programatically create gbut with link-to-directory."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-directory "/tmp"))
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
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line-and-column :args (list test-file 10 20) :loc test-file :lbl-key"global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest hypb:program-create-ebut-in-buffer ()
  "Create button with hypb:program in buffer."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
    (hbut-tests:should-match-tmp-folder (car (hattr:get (hbut:at-p) 'args)))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))))

(ert-deftest hypb:program-create-ebut-in-buffer-with-same-label ()
  "Create button with same label shall add number so it is unique."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))
    (ebut:program "label" 'link-to-directory "/tmp")
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
    (ibut:program "label" 'link-to-directory "/tmp")
    (should (string= "<[label]> - \"/tmp\"" (buffer-string)))))

(ert-deftest hbut-tests-ibut-program-link-to-file ()
  "Programatically create ibut link to file."
  (let ((test-file (make-temp-file "ibut" nil ".txt")))
    (unwind-protect
        (with-temp-buffer
          (ibut:program "label" 'link-to-file test-file)
          (should (string=
                   (concat "<[label]> - \"" test-file "\"")
                   (buffer-string))))
      (delete-file test-file))))

(ert-deftest hbut-tests-ibut-insert-text-link-to-dir ()
  "Insert link to dir."
  (with-temp-buffer
    (ibut:program "label" 'link-to-directory "/tmp")
    (should (string= "<[label]> - \"/tmp\"" (buffer-string)))
    (goto-char 3)
    (let ((but (ibut:at-p)))
      (with-temp-buffer
        (ibut:insert-text but)
	;; Allow for /tmp being a link to /private/tmp on Macos
        (should (string-match "\"\\(/private\\)?/tmp\"" (buffer-string)))))))

(ert-deftest hbut-tests-ibut-insert-annot-bib ()
  "Insert ibut to annot-bib, which must be attached to a file."
  (let ((annot-bib-file (make-temp-file "annot-bib" nil ".txt"))
	annot-bib-buf)
    (unwind-protect
        (progn
	  ;; Test with name
          (setq annot-bib-buf (find-file annot-bib-file))
	  (ibut:program "label" 'annot-bib "arg")
	  (save-buffer)
	  (should (string-match (concat (regexp-quote "<[label]> - [arg]")
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
	  (ibut:program "label" 'kbd-key "{ C-f C-f }")
	  (save-buffer)
	  (should (string-match (concat (regexp-quote "<[label]> - { C-f C-f }")
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
		   ;; These shoulds show the value of these variables when you
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
  "Create unnamed ibut.
  |------+----------+--------+-----------+-----------------------------------------------|
  | name | new-name | region | edit-flag | operation                                     |
  |------+----------+--------+-----------+-----------------------------------------------|
  | nil  | nil      | nil    | nil       | create: unnamed ibut from hbut:current attrs  |
  |------+----------+--------+-----------+-----------------------------------------------|"
  (with-temp-buffer
    (insert "/tmp")
    (goto-char 2)
    (should (hbut:at-p))
    (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))
    (erase-buffer)
    (should-not (ibut:operate))
    (should (hbut:at-p))
    (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))))

(ert-deftest hbut-tests--ibut-operate--aname ()
  "Create aname ibut."
  (with-temp-buffer
    (insert "<[aname]> - /tmp")
    (goto-char 2)
    (should (hbut:at-p))
    (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    (hbut-tests:should-match-tmp-folder (buffer-substring-no-properties (point-min) (point-max)))
    (erase-buffer)
    (
     (hattr:set 'hbut:current 'name "aname")
     (hattr:set 'hbut:current 'name "")
    (should-not (ibut:operate))
    (should (hbut:at-p))
    (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    (should (string= "<[aname]> - /tmp<[aname]> - \"/tmp\""
		     (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest hbut-tests--ibut-operate--aname-region-skip-region ()
  "Create aname ibut and ignore region."
  (with-temp-buffer
    (insert "<[aname]> - /tmp")
    (goto-char 2)
    (should (hbut:at-p))
    (end-of-buffer)
    (insert "\n")
    (set-mark (point))
    (insert "abcd")
    (should (region-active-p))
    (should-not (ibut:operate))
    ;; Inserted just before region which is kept
    (should (string= "<[aname]> - /tmp\n<[aname]> - \"/tmp\"abcd"
		     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--region ()
  "Create ibut with aname, ignore region."
  (with-temp-buffer
    (insert "/tmp")
    (goto-char 2)
    (should (hbut:at-p))
    (end-of-buffer)
    (insert "\n")
    (set-mark (point))
    (insert "name")
    (should (region-active-p))
    (should-not (ibut:operate))
    (should (string= "/tmp\n<[name]>\"/tmp\""
		     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--modify-named ()
  "Add new-name to named ibut."
  (with-temp-buffer
    (insert "<[name]> /tmp")
    (goto-char 2)
    (should (hbut:at-p))
    (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    (should-not (ibut:operate "new-name" t))
    (should (hbut:at-p))
    (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    (should (string= "<[new-name]> /tmp"
		     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--modify-named-skip-region ()
  "Add new-name to named ibut and ignore region."
  (with-temp-buffer
    (insert "<[name]> /tmp")
    (goto-char 2)
    (should (hbut:at-p))
    (set-mark (point-max))
    (should (region-active-p))
    (should-not (ibut:operate "new-name" t))
    (should (hbut:at-p))
    (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    (should (string= "<[new-name]> /tmp"
		     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--add-new-name ()
  "Add new-name to unnamed ibut."
  (with-temp-buffer
    (insert "/tmp")
    (goto-char 2)
    (should (hbut:at-p))
    (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    (should-not (ibut:operate "new-name" t))
    ;; Missing delimiter -- Not identified as a ibut after name is inserted
    ;; (should (hbut:at-p))
    ;; (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    ;; delimiter
    (should (string= "<[new-name]>/tmp"
		     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hbut-tests--ibut-operate--add-new-name-skip-region ()
  "Add new-name to unnamed ibut, skip active region."
  (with-temp-buffer
    (insert "/tmp")
    (goto-char 2)
    (should (hbut:at-p))
    (set-mark (point-max))
    (should (region-active-p))
    (should-not (ibut:operate "new-name" t))
    ;; Missing delimiter -- Not identified as a ibut after name is inserted
    ;; (should (hbut:at-p))
    ;; (should (eq (hattr:get 'hbut:current 'actype) 'actypes::link-to-file))
    ;; Missing delimiter
    (should (string= "<[new-name]>/tmp"
		     ))))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hbut-tests)
;;; hbut-tests.el ends here
