;;; demo-tests.el --- unit tests from examples in the DEMO         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:     24-Sep-22 at 12:40:19 by Bob Weiner
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Runs unit tests on some of the examples given in the DEMO file.

;;; Code:

(require 'ert)
(require 'hib-kbd)
(require 'hmouse-drv)
(require 'hhist)
(require 'hload-path)
(require 'hypb)
(require 'hib-social)
(require 'hyrolo)
(require 'eww)
(require 'compile)
(require 'el-mock)
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")
(declare-function hy-test-helpers:should-last-message "hy-test-helpers")
(declare-function hyrolo-demo-quit "hyrolo-demo.el")
(declare-function org-check-for-hidden "org-el")

(ert-deftest demo-smart-mouse-keys-ref-test ()
  "Go to the header from a #ref."
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "#Smart Keys")
        (action-key)
        (should (bolp))
        (should (looking-at "^\\* Smart")))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest demo-smart-mouse-keys-ebut-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "<(Smart")
        (action-key)
        (should (bolp))
        (should (looking-at "^ +\\* Smart")))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest demo-table-of-contents-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward " \\* Koutl")
        (action-key)
        (should (bolp))
        (should (looking-at "^* Koutliner")))
    (hy-test-helpers:kill-buffer "DEMO")))

;; Smart scrolling
(ert-deftest demo-smart-scrolling-proportional-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "Table of Contents")
        (beginning-of-line)
        (let ((smart-scroll-proportional t)
              (pos (point)))
          (end-of-line)
          (action-key)
          (should (= pos (window-start)))))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest demo-smart-scrolling-non-proportional-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "Table of Contents")
        (beginning-of-line)
        (let ((smart-scroll-proportional nil)
              (pos (window-start)))
          (end-of-line)
          (action-key)
          (should (< pos (window-start)))))
    (hy-test-helpers:kill-buffer "DEMO")))

;; Hyperbole menus

;; Help Buffer
(ert-deftest demo-action-key-help ()
  (let ((help-buffer "*Help: Hyperbole Action Key*"))
    (hy-test-helpers:kill-buffer help-buffer)
    (unwind-protect
        (with-temp-buffer
          (insert "Text")
          (hkey-help)
          (should (get-buffer help-buffer))
      (hy-test-helpers:kill-buffer help-buffer)))))

(ert-deftest demo-assist-key-help ()
  (let ((help-buffer "*Help: Hyperbole Assist Key*"))
    (hy-test-helpers:kill-buffer help-buffer)
    (unwind-protect
        (with-temp-buffer
          (insert "Text")
          (hkey-help t)
          (should (get-buffer help-buffer))
      (hy-test-helpers:kill-buffer help-buffer)))))

;; HyControl
(ert-deftest demo-window-grid-22-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (should (hact 'kbd-key "C-h h s f @ 22 RET Q"))
        (hy-test-helpers:consume-input-events)
        (should (eq 4 (length (window-list)))))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest demo-window-grid-33-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (should (hact 'kbd-key "C-h h s f @ 33 RET Q"))
        (hy-test-helpers:consume-input-events)
        (should (eq 9 (length (window-list)))))
    (hy-test-helpers:kill-buffer "DEMO")))

;; HyRolo
(ert-deftest demo-hyrolo-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (with-temp-buffer
        (load (expand-file-name "hyrolo-demo.el" hyperb:dir))
        (should (hact 'kbd-key "C-x 4 r work RET"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) hyrolo-display-buffer))
        (should (search-forward "Dunn, John")))
    (hyrolo-demo-quit)))

;; Info
(ert-deftest demo-hy-info-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "\"(hyperbole)HyRolo Keys\"")
        (goto-char 5)
        (action-key)
        (should (string= "*info*" (buffer-name))))
    (hy-test-helpers:kill-buffer "*info*")))

;; History
(ert-deftest demo-hy-history-test ()
  (unwind-protect
      (with-temp-buffer
        (let ((tmp-buf-name (buffer-name))
              (pm))
          (hhist:init)
          (insert "\"(hyperbole)HyRolo Keys\"")
          (goto-char 5)
          (setq pm (point-marker))
          (action-key)
          (should (string= "*info*" (buffer-name)))
          (hhist:remove)
          (should (string= tmp-buf-name (buffer-name)))
          (should (equal pm (point-marker)))
          ))
    (hy-test-helpers:kill-buffer "*info*")))

;; Implicit Buttons
(ert-deftest demo-implicit-button-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s\"" (expand-file-name "DEMO" hyperb:dir)))
        (goto-char 2)
        (action-key)
        (should (string= "DEMO" (buffer-name))))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest demo-implicit-button-action-button-action-type-invocation-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "<link-to-file-line \"${hyperb:dir}/DEMO\" 5>")
        (goto-char 5)
        (action-key)
        (should (string= "DEMO" (buffer-name)))
        (should (= 5 (line-number-at-pos (point)))))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest demo-implicit-button-action-button-function-call-test ()
  (with-temp-buffer
    (insert "<message \"%d\" (eval (+ 2 2))>")
    (goto-char 2)
    (action-key)
    (hy-test-helpers:should-last-message "4")))

(ert-deftest demo-implicit-button-action-button-sexp-test ()
  (with-temp-buffer
    (insert
     "<progn (mapc (lambda (f) (bury-buffer (find-file-noselect
                                             (expand-file-name f hyperb:dir))))
		   '(\"hibtypes.el\" \"hactypes.el\" \"hsettings.el\"))
	     (message \"Last 3 buffers are: %S\"
		      (mapcar #'buffer-name
			      (nthcdr (- (length (buffer-list)) 3) (buffer-list))))>")
    (goto-char 2)
    (action-key)
    (let* ((bufs (reverse (buffer-list)))
 	   (hsettings-buf (buffer-name (nth 0 bufs)))
	   (hactypes-buf  (buffer-name (nth 1 bufs)))
	   (hibtypes-buf  (buffer-name (nth 2 bufs))))
      (should (and (hy-test-helpers:should-last-message "Last 3 buffers are")
		   (string-match-p "hsettings\\.el" hsettings-buf)
		   (string-match-p "hactypes\\.el"  hactypes-buf)
		   (string-match-p "hibtypes\\.el"  hibtypes-buf))))))

(ert-deftest demo-implicit-button-action-button-boolean-function-call-test ()
  (with-temp-buffer
    (insert "<string-empty-p \"False\">")
    (goto-char 2)
    (action-key)
    (hy-test-helpers:should-last-message "Boolean result (False) = nil")))

(ert-deftest demo-implicit-button-action-button-variable-display-test ()
  (with-temp-buffer
    (insert "<fill-column>")
    (goto-char 2)
    (action-key)
    (hy-test-helpers:should-last-message (format "fill-column = %d" (current-fill-column)))))

(ert-deftest demo-implicit-button-hash-link-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s%s\"" (expand-file-name "README.md" hyperb:dir) "#why-was-hyperbole-developed"))
        (goto-char 5)
        (action-key)
        (should (string= "README.md" (buffer-name)))
        (should (looking-at "## Why was Hyperbole developed\\?")))
    (hy-test-helpers:kill-buffer "README.md")))

(ert-deftest demo-implicit-button-line-and-column-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s%s\"" (expand-file-name "HY-ABOUT" hyperb:dir) ":5:46"))
        (goto-char 5)
        (action-key)
        (should (string= "HY-ABOUT" (buffer-name)))
        (should (looking-at "hyperbole/")))
    (hy-test-helpers:kill-buffer "HY-ABOUT")))

;; org
(ert-deftest demo-org-hide-header-test ()
  "Hide org mode header."
  (with-temp-buffer
    (org-mode)
    ;; Without this next let, default Org behavior may occur which
    ;; inserts a new heading when org-meta-return is called rather
    ;; than collapsing the existing tree of headings.
    (let ((hsys-org-enable-smart-keys t))
      (insert "* 1\n** 2\n*** 3\n")
      (goto-char 1)
      (should (not (org-check-for-hidden 'headlines)))
      (save-excursion
	(action-key))
;;; (org-hide-entry)
      (should (org-check-for-hidden 'headlines)))))

;; Manifest
(ert-deftest demo-manifest-test ()
  (unwind-protect
      (progn
        (find-file (expand-file-name "MANIFEST" hyperb:dir))
        (beginning-of-buffer)
        (forward-line 1)
        (should (looking-at "COPYING"))
        (action-key)
        (should (string= "COPYING" (buffer-name)))
        (should (looking-at ".*GNU GENERAL PUBLIC LICENSE")))
    (progn
      (hy-test-helpers:kill-buffer "MANIFEST")
      (hy-test-helpers:kill-buffer "COPYING"))))

;; Email compose
(ert-deftest demo-mail-compose-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "receiver@mail.org")
        (goto-char 2)
        (action-key)
        (should (string= "*mail*" (buffer-name))))
    (hy-test-helpers:kill-buffer "*mail*")))


(defun demo-should-browse-twitter-url (url &optional new-window)
  "Verify call with proper URL and optional NEW-WINDOW."
  (should (equal url "https://twitter.com/search?q=@fsf"))
  (should (equal new-window nil)))

;; Social
(ert-deftest demo-social-twitter-test ()
  (with-temp-buffer
    (insert "tw@fsf")
    (goto-char 2)
    (let ((browse-url-browser-function 'demo-should-browse-twitter-url))
      (action-key))))


(defun demo-should-browse-github-url (url &optional new-window)
  "Verify call with proper URL and optional NEW-WINDOW."
  (should (equal url "https://github.com/rswgnu/hyperbole"))
  (should (equal new-window nil)))

;; WWW
(ert-deftest demo-www-test ()
  (with-temp-buffer
    (insert "https://github.com/rswgnu/hyperbole")
    (goto-char 4)
    (let ((browse-url-browser-function 'demo-should-browse-github-url)
          (hibtypes-github-default-user "rswgnu"))
      (action-key))))

;; Github
(ert-deftest demo-github-user-default-test ()
  (with-temp-buffer
    (insert "gh#/hyperbole")
    (goto-char 4)
    (let ((browse-url-browser-function 'demo-should-browse-github-url)
          (hibtypes-github-default-user "rswgnu"))
      (action-key))))

(ert-deftest demo-github-ignore-default-test ()
  (with-temp-buffer
    (insert "gh#/rswgnu/hyperbole")
    (goto-char 4)
    (let ((browse-url-browser-function 'demo-should-browse-github-url)
          (hibtypes-github-default-user "whatever"))
      (action-key))))

;; Occur
(ert-deftest demo-occur-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (should (hact 'kbd-key "C-h h f o Hyperbole RET"))
        (hy-test-helpers:consume-input-events)
        (set-buffer "*Occur*")
        (should (looking-at "[0-9]+ matches in [0-9]+ lines for \"Hyperbole\" in buffer: DEMO")))
    (progn
      (hy-test-helpers:kill-buffer "DEMO")
      (hy-test-helpers:kill-buffer "*Occur*"))))

;; Annotated references
(ert-deftest demo-annotated-reference-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (re-search-forward "\\[FSF 19\\]")
        (backward-char 1)
        (action-key)
        (should (looking-at "\\[FSF 19\\] Free Software Foundation"))
        (forward-line -2)
        (should (looking-at "\\* References")))
    (hy-test-helpers:kill-buffer "DEMO")))

;; Man appropos
(ert-deftest demo-man-appropos-test ()
  (with-temp-buffer
    (insert "rm (1)   - remove")
    (goto-char 4)
    (with-mock
      (mock (man "rm(1)") => t)
      (action-key))))

;; Explicit buttons
(ert-deftest demo-factorial-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (should (hact 'kbd-key "C-h h a factorial RET"))
        (hy-test-helpers:consume-input-events)
        (hy-test-helpers:should-last-message "Factorial of 5 = 120"))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest demo-factorial-ebutton-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (re-search-forward "<(factorial)>")
        (forward-char -5)
        (action-key)
        (hy-test-helpers:should-last-message "Factorial of 5 = 120"))
    (hy-test-helpers:kill-buffer "DEMO")))

;; Fast demo key series
(ert-deftest fast-demo-key-series-help-buffer ()
  "Action key on C-hA brings up help buffer for action key."
  (let ((help-buffer "*Help: Hyperbole Action Key*"))
    (unwind-protect
        (with-temp-buffer
          (insert "{C-h A}")
          (goto-char 3)
          (action-key)
          (should (get-buffer help-buffer)))
      (hy-test-helpers:kill-buffer help-buffer))))

(ert-deftest fast-demo-key-series-window-grid-22 ()
  "Action key on window grid key series creates a grid."
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (insert "{C-c @ 22 RET}")
    (goto-char 3)
    (action-key)
    (hy-test-helpers:consume-input-events)
    (should (= 4 (length (window-list))))))

(ert-deftest fast-demo-key-series-kotl-files ()
  "Action key brings up kotl files in a grid.
Note: Depends on ket series in FAST-DEMO and how many files in
hyberbole folder that starts with kotl."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "FAST-DEMO")
        (search-forward "{C--1 C-c")
        (action-key)
        (hy-test-helpers:consume-input-events)
        (should (= 4 (length (window-list)))))
    (hy-test-helpers:kill-buffer "FAST-DEMO")))

(ert-deftest fast-demo-key-series-emacs-lisp-mode ()
  "Action key brings up `emacs-lisp-mode' files in a grid.
Note: Relies on that empty windows are created when there are not
enough files with matching mode loaded."
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (insert "{C-u 0 C-c @ emacs-lisp-mode RET 33 RET}")
    (goto-char 3)
    (action-key)
    (hy-test-helpers:consume-input-events)
    (should (= 9 (length (window-list))))))

(ert-deftest fast-demo-key-series-hyperbole-dir ()
  "Action key on hyperb:dir brings up hyperbole folder."
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (insert "{C-x 4 d ${hyperb:dir} RET}")
    (goto-char 5)
    (action-key)
    (hy-test-helpers:consume-input-events)
    (should (equal 'dired-mode major-mode))
    (should (equal hyperb:dir (expand-file-name default-directory)))))

(ert-deftest fast-demo-key-series-keep-lines-ext ()
  "Action key opens Ibuffer and keep lines with extension."
  (skip-unless (not noninteractive))
  (let ((buff "*Ibuffer*")
        (old (global-key-binding (kbd "C-x C-b")))
        (tmp (make-temp-file "hypb" nil ".hypb-test")))
    (unwind-protect
        (with-temp-buffer
          (global-set-key (kbd "C-x C-b") 'ibuffer)
          (find-file-noselect tmp)
          (insert "{C-x C-b C-x C-q M-x keep-lines RET .hypb-test$ RET C-x C-q}")
          (goto-char 5)
          (action-key)
          (hy-test-helpers:consume-input-events)
          (with-current-buffer buff
            (should (looking-at-p (concat ".*" tmp)))))
      (hy-test-helpers:kill-buffer buff)
      (global-set-key (kbd "C-x C-b") old)
      (hy-test-helpers:kill-buffer (get-file-buffer tmp))
      (delete-file tmp))))

(ert-deftest fast-demo-key-series-keep-lines-slash ()
  "Action key opens Ibuffer and keep lines that contains a slash."
  (skip-unless (not noninteractive))
  (let ((buff "*Ibuffer*")
        (old (global-key-binding (kbd "C-x C-b")))
        (dir (dired hyperb:dir)))
    (unwind-protect
        (with-temp-buffer
          (global-set-key (kbd "C-x C-b") 'ibuffer)
          (insert "{C-x C-b C-x C-q M-x keep-lines RET [\\/]$ RET C-x C-q}")
          (goto-char 5)
          (action-key)
          (hy-test-helpers:consume-input-events)
          (with-current-buffer buff
            (should (looking-at-p (concat ".*[\\/]")))))
      (hy-test-helpers:kill-buffer buff)
      (global-set-key (kbd "C-x C-b") old)
      (hy-test-helpers:kill-buffer dir))))

(ert-deftest fast-demo-key-series-keep-lines-dired ()
  "Action key opens Ibuffer and keep `dired-mode' lines."
  (skip-unless (not noninteractive))
  (let ((buff "*Ibuffer*")
        (old (global-key-binding (kbd "C-x C-b")))
        (dir (dired hyperb:dir)))
    (unwind-protect
        (with-temp-buffer
          (global-set-key (kbd "C-x C-b") 'ibuffer)
          (insert "{C-x C-b / RET dired-mode RET}")
          (goto-char 5)
          (action-key)
          (hy-test-helpers:consume-input-events)
          (with-current-buffer buff
            (should (looking-at-p (concat ".*Dired by name")))))
      (hy-test-helpers:kill-buffer buff)
      (global-set-key (kbd "C-x C-b") old)
      (hy-test-helpers:kill-buffer dir))))

(ert-deftest fast-demo-key-series-shell-cd-hyperb-dir ()
  "Action key executes cd shell command."
  (skip-unless (not noninteractive))
  (let* ((shell-file-name (executable-find "sh"))
         (shell-buffer-name "*shell*")
	 (existing-shell-flag (get-buffer-process shell-buffer-name)))
    (unwind-protect
        (with-temp-buffer
          (insert "{ M-x shell RET M-> (cd ${hyperb:dir} && echo \"PWD=$(pwd)\") RET }")
          (goto-char 5)
          (action-key)
          (hy-test-helpers:consume-input-events)
          (with-current-buffer shell-buffer-name
            (goto-char (point-min))
            (end-of-line)
            (with-timeout (5 (ert-fail "Test timed out"))
              (while (not (search-forward "PWD=" nil t))
                (accept-process-output (get-buffer-process shell-buffer-name))))
            (should (looking-at-p (directory-file-name hyperb:dir)))))
      (unless existing-shell-flag
	(set-process-query-on-exit-flag (get-buffer-process shell-buffer-name) nil)
	(hy-test-helpers:kill-buffer shell-buffer-name)))))

(ert-deftest fast-demo-key-series-shell-grep ()
  "Action key executes grep shell command."
  (skip-unless (not noninteractive))
  (let* ((shell-file-name (executable-find "sh"))
         (shell-buffer-name "*shell*")
	 (existing-shell-flag (get-buffer-process shell-buffer-name)))
    (unwind-protect
        (with-temp-buffer
          (insert "{M-x shell RET M-> (export HYPERBOLE_DIR=${hyperb:dir} && cd $HYPERBOLE_DIR && grep -n gbut:label-list *.el) RET}")
          (goto-char 5)
          (action-key)
          (hy-test-helpers:consume-input-events)
          (with-current-buffer shell-buffer-name
            (with-timeout (5 (ert-fail "Test timed out"))
              (while (not (string-match-p "\n.*\\.el:[0-9]+:.*defun.*gbut:label-list ()" (buffer-substring-no-properties (point-min) (point-max))))
                (accept-process-output (get-buffer-process shell-buffer-name))))
            (should (string-match-p "\n.*\\.el:[0-9]+:.*defun.*gbut:label-list ()" (buffer-substring-no-properties (point-min) (point-max))))))
      (unless existing-shell-flag
	(set-process-query-on-exit-flag (get-buffer-process shell-buffer-name) nil)
	(hy-test-helpers:kill-buffer shell-buffer-name)))))

(ert-deftest fast-demo-key-series-shell-apropos ()
  "Action key executes apropos shell command."
  (skip-unless (not noninteractive))
  (let* ((shell-file-name (executable-find "sh"))
         (shell-buffer-name "*shell*")
	 (existing-shell-flag (get-buffer-process shell-buffer-name)))
    (unwind-protect
        (with-temp-buffer
          (insert "{M-x shell RET M-> (apropos grep) RET RET}")
          (goto-char 5)
          (action-key)
          (hy-test-helpers:consume-input-events)
          (with-current-buffer shell-buffer-name
            (with-timeout (5 (ert-fail "Test timed out"))
              (while (not (string-match-p "grep ?(1).*-" (buffer-substring-no-properties (point-min) (point-max))))
                (accept-process-output (get-buffer-process shell-buffer-name))))
            (should (string-match-p "grep ?(1).*-" (buffer-substring-no-properties (point-min) (point-max))))))
      (unless existing-shell-flag
	(set-process-query-on-exit-flag (get-buffer-process shell-buffer-name) nil)
	(hy-test-helpers:kill-buffer shell-buffer-name)))))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'demo-tests)
;;; demo-tests.el ends here
