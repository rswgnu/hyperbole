;;; hbut-tests.el --- hbut unit tests         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-may-21 at 09:33:00
;; Last-Mod:     23-Jul-22 at 20:04:45 by Bob Weiner
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

(defun hbut-tests:should-match-tmp-folder (tmp)
  "Check that TMP matches either a list of a single element of \"/tmp\" or \"private/tmp\".
Needed since hyperbole expands all links to absolute paths and
/tmp can be a symbolic link."
  (should (member tmp '(("/tmp") ("./tmp") ("/private/tmp")))))

(ert-deftest ebut-program-link-to-directory ()
  "Programatically create ebut with link-to-directory."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory "/tmp")
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
          (hbut-tests:should-match-tmp-folder (hattr:get (hbut:at-p) 'args))
          (should (equal (hattr:get (hbut:at-p) 'loc) file))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest ebut-program-link-to-directory-2 ()
  "Programatically create ebut with link-to-directory using `temporary-file-directory`."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory temporary-file-directory)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args (list temporary-file-directory) :loc file :lbl-key "label"))
      (delete-file file))))

(ert-deftest ebut-program-shell-cmd ()
  "Programatically create ebut running shell command."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'exec-shell-cmd "ls /tmp")
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::exec-shell-cmd :args '("ls /tmp") :loc file :lbl-key "label"))
      (delete-file file))))

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
      (delete-file file))))

(ert-deftest gbut-program-calls-ebut-program ()
  "Programatically create gbut calls ebut:program."
  (let ((test-file (make-temp-file "test-file")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
        (with-mock
          (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
          (mock (ebut:program "label" 'link-to-directory "/tmp") => t)
          (gbut:ebut-program "label" 'link-to-directory "/tmp"))
      (delete-file test-file))))

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
            (hbut-tests:should-match-tmp-folder (hattr:get (hbut:at-p) 'args))
            (should (equal (hattr:get (hbut:at-p) 'loc) test-file))
            (should (equal (hattr:get (hbut:at-p) 'lbl-key) "global"))))
      (delete-file test-file))))

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
      (delete-file test-file))))

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
      (delete-file test-file))))

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
      (delete-file test-file))))

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
      (delete-file test-file))))

(ert-deftest hypb:program-create-ebut-in-buffer ()
  "Create button with hypb:program in buffer."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
    (hbut-tests:should-match-tmp-folder (hattr:get (hbut:at-p) 'args))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))))

(ert-deftest hypb:program-create-link-to-file-line-and-column-but-in-file ()
  "Create button that links to file with line and column with hypb:program in buffer."
  (let ((test-file (make-temp-file "test-file")))
    (unwind-protect
        (progn
          (find-file test-file)
          (ebut:program "label" 'link-to-file-line-and-column test-file 2 3)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line-and-column :args (list test-file 2 3) :loc test-file :lbl-key "label"))
      (delete-file test-file))))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hbut-tests)
;;; hbut-tests.el ends here
