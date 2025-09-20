;;; hui-mouse-tests.el --- unit tests for hui-mouse -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    15-Mar-25 at 22:39:37
;; Last-Mod:     20-Sep-25 at 01:16:24 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025 Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Unit tests for "../hui-mouse.el".

;;; Code:

(require 'ert)
(require 'el-mock)

(ert-deftest hui-mouse-tests--hkey-alist ()
  "Verify that given predicate values result in the proper action."
  ;; Mode predicates where only the mode matters for the selection.
  (let ((mode-list
         '((treemacs-mode . ((smart-treemacs) . (smart-treemacs)))
           (dired-sidebar-mode . ((smart-dired-sidebar) . (smart-dired-sidebar)))
           ;; Smart Menu system. Part of InfoDock but not part of Hyperbole.
           (smart-menu-mode . ((smart-menu-select) . (smart-menu-help)))
           ;; Dired mode derivatives.
           (dired-mode . ((smart-dired) . (smart-dired-assist)))
           (magit-status-mode . ((smart-magit) . (smart-magit-assist)))
           (occur-mode . ((occur-mode-goto-occurrence) . (occur-mode-goto-occurrence)))
           (moccur-mode . ((moccur-mode-goto-occurrence) . (moccur-mode-goto-occurrence)))
           (amoccur-mode . ((amoccur-mode-goto-occurrence) . (amoccur-mode-goto-occurrence)))
           (kotl-mode . ((kotl-mode:action-key) . (kotl-mode:assist-key)))
           (flymake-diagnostics-buffer-mode . ((flymake-goto-diagnostic (point)) . (flymake-show-diagnostic (point) t)))
           (rdb-mode . ((rdb:action-key) . (rdb:assist-key)))
           (Custom-mode . ((smart-custom) . (smart-custom-assist)))
           (bookmark-bmenu-mode . ((bookmark-jump (bookmark-bmenu-bookmark) (hpath:display-buffer-function)) . (hkey-help)))
           (pages-directory-mode . ((pages-directory-goto) . (pages-directory-goto)))
           (calendar-mode . ((smart-calendar) . (smart-calendar-assist)))
           (unix-apropos-mode . ((smart-apropos) . (smart-apropos-assist)))
           (outline-mode . ((smart-outline) . (smart-outline-assist)))
           (Info-mode . ((smart-info) . (smart-info-assist)))
           (gnus-group-mode . ((smart-gnus-group) . (smart-gnus-group-assist)))
           (gnus-summary-mode . ((smart-gnus-summary) . (smart-gnus-summary-assist)))
           (gnus-article-mode . ((smart-gnus-article) . (smart-gnus-article-assist)))
           (Buffer-menu-mode . ((smart-buffer-menu) . (smart-buffer-menu-assist)))
           (ibuffer-mode . ((smart-ibuffer-menu) . (smart-ibuffer-menu-assist)))
           (tar-mode . ((smart-tar) . (smart-tar-assist)))
           (w3-mode . ((w3-follow-link) . (w3-goto-last-buffer)))
           ;; (hynote-mode . ((smart-hynote) . (smart-hynote-assist)))     ; Hy-note -- Not yet released
           (hyrolo-mode . ((smart-hyrolo) . (smart-hyrolo-assist)))
           (image-dired-thumbnail-mode . ((smart-image-dired-thumbnail) . (smart-image-dired-thumbnail-assist)))
           (gomoku-mode . ((gomoku-human-plays) . (gomoku-human-takes-back)))
           (todotxt-mode . ((smart-todotxt) . (smart-todotxt-assist))))))
    (dolist (mode mode-list)
      (let ((major-mode (car mode)))
        (should (equal (hkey-actions)
                       (cdr mode))))))

  ;; Predicates not based on mode or using other guard expressions.
  ;; Company completion mode
  (defvar company-active-map)
  (let ((company-active-map 'value))
    (mocklet (((current-minor-mode-maps) => (list company-active-map))
              ((boundp 'company-active-map) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-company-to-definition) '(smart-company-help))))))

  ;; Handle Emacs push buttons in buffers
  (mocklet (((button-at (point)) => t))
    (should (equal (hkey-actions)
                   (cons '(smart-push-button nil (mouse-event-p last-command-event))
	                 '(smart-push-button-help nil (mouse-event-p last-command-event))))))

  ;; Vertico
  (defvar vertico-mode)
  (defvar ivy-mode)
  (let (ivy-mode
        (vertico-mode t))
    (mocklet ((vertico--command-p => t))
      (should (equal (hkey-actions)
		     (cons '(funcall (lookup-key vertico-map (kbd "M-RET")))
			   '(funcall (lookup-key vertico-map (kbd "M-RET"))))))))

  ;; If in the minibuffer and reading a non-menu Hyperbole argument
  ;; (aside from with vertico or ivy), accept the argument or give
  ;; completion help.
  (let ((hargs:reading-type 'non-hyperbole-menu)
        ivy-mode
        vertico-mode)
    (mocklet (((minibuffer-depth) => 1)
              ((selected-window) => (minibuffer-window))
              ((smart-helm-alive-p) => nil))
      (should (equal (hkey-actions)
                     (cons '(funcall (key-binding (kbd "RET")))
                           '(smart-completion-help))))))

  ;; If reading a Hyperbole menu item or a Hyperbole completion-based
  ;; argument, allow selection of an item at point.
  (mocklet (((hargs:at-p) => 'thing-at-point)
            ((minibuffer-depth) => 1))
    (should (equal (hkey-actions)
                   (cons '(hargs:select-p hkey-value) '(hargs:select-p hkey-value 'assist)))))

  ;; If reading a Hyperbole menu item and nothing is selected, just
  ;; return.  Or if in a helm session with point in the minibuffer,
  ;; quit the session and activate the selected item.
  (mocklet (((minibuffer-depth) => 1)
            ((selected-window) => (minibuffer-window)))
    (let ((hargs:reading-type 'hmenu))
      (should (equal (hkey-actions)
                     (cons '(funcall (key-binding (kbd "RET"))) '(funcall (key-binding (kbd "RET")))))))
    (let (hargs:reading-type)
      (mocklet (((smart-helm-alive-p) => t))
        (should (equal (hkey-actions)
                       (cons '(funcall (key-binding (kbd "RET"))) '(funcall (key-binding (kbd "RET")))))))))

  ;; EOL
  (mocklet (((smart-eolp) => t))
    ;; Not in org mode.
    (let (hsys-org-enable-smart-keys
          (hsys-org-mode-function #'hsys-org-mode-p))
      (mocklet (((hsys-org-mode-p) => nil))
        (should (equal (hkey-actions)
                       (cons '(hact action-key-eol-function) '(hact assist-key-eol-function))))))
    ;; With smart-keys active in org-mode.
    (let ((hsys-org-enable-smart-keys t)
          (hsys-org-mode-function #'hsys-org-mode-p))
      (mocklet (((hsys-org-mode-p) => t))
        (should (equal (hkey-actions)
                       (cons '(hact action-key-eol-function) '(hact assist-key-eol-function)))))))

  ;; Handle any Org mode-specific contexts but give priority to
  ;; Hyperbole buttons prior to cycling Org headlines
  (mocklet (((hyperb:stack-frame '(smart-org)) => nil)
            ((smart-org) => t))
    (should (equal (hkey-actions)
                   (cons '(smart-org) '(smart-org)))))

  ;; The ID-edit package supports rapid killing, copying, yanking and
  ;; display management. It is available only as a part of InfoDock.
  ;; It is not included with Hyperbole.
  (defvar id-edit-mode)
  (let ((id-edit-mode t)
        buffer-read-only)
    (mocklet (((smart-helm-alive-p) => nil))
      (should (equal (hkey-actions)
                     (cons '(id-edit-yank) '(id-edit-yank))))))

  ;; If in an xref buffer on a listing of matching identifier lines,
  ;; go to the source line referenced by the current entry.
  (mocklet (((hsys-xref-item-at-point) => t))
    (should (equal (hkey-actions)
                   (cons '(xref-goto-xref) '(xref-show-location-at-point)))))

  ;; Hyperbole buttons
  (mocklet (((hbut:at-p) => t))
    (should (equal (hkey-actions)
                   (cons '(hui:hbut-act 'hbut:current) '(hui:hbut-help 'hbut:current)))))

  ;; View minor mode
  (let ((view-mode t))
    (should (equal (hkey-actions)
                   (cons '(cond ((last-line-p)
	                         (view-quit))
	                        ((pos-visible-in-window-p (point-max))
	                         (goto-char (point-max)))
	                        (t (View-scroll-page-forward)))
	                 '(View-scroll-page-backward)))))

  ;; Direct access selection of helm-major-mode completions
  (let ((major-mode 'helm-major-mode))
    (mocklet (((eolp) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-helm) '(smart-helm-assist)))))
    (mocklet (((eolp) => nil)
              ((smart-helm-at-header) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-helm) '(smart-helm-assist))))))

  ;; Ert
  (let ((major-mode 'ert-results-mode))
    (mocklet (((ert-results-filter-status-p) => t))
      (cl-letf (((symbol-function 'featurep)
                 (lambda (symbol &optional _) (equal symbol 'ert-results))))
        (should (equal (hkey-actions)
                       (cons '(smart-ert-results hkey-value)
                             '(smart-ert-results-assist hkey-value)))))))

  ;; OO-Browser
  (mocklet (((br-in-browser) => t))
    (should (equal (hkey-actions)
                   (cons '(smart-br-dispatch)
                         '(smart-br-assist-dispatch)))))
  (mocklet (((br-in-browser) => nil))
    (let ((major-mode 'br-mode))
      (should (equal (hkey-actions)
                     (cons '(smart-br-dispatch)
                           '(smart-br-assist-dispatch))))))

  ;; Select or select-and-kill a markup pair ...
  (mocklet (((hui-select-at-delimited-thing-p) => t))
    (should (equal (hkey-actions)
                   (cons '(hui-select-thing)
                         '(progn (hui-select-thing)
				 (hmouse-kill-region))))))

  ;; sexpression
  (mocklet (((hui-select-at-delimited-sexp-p) => t))
    (should (equal (hkey-actions)
                   (cons '(hui-select-mark-delimited-sexp)
	                 '(progn (hui-select-mark-delimited-sexp)
                                 (hmouse-kill-region))))))

  ;; Restore window config and hide help buffer when click at buffer end.
  (mocklet (((point-max) => (point)))
    (cl-letf (((symbol-function 'buffer-name) (lambda (&optional _) "*Help*")))
      (should (equal (hkey-actions)
                     (cons '(hkey-help-hide) '(hkey-help-hide))))))

  ;; Any other programming mode
  (mocklet (((smart-prog-at-tag-p) => t)
	    ((smart-tags-find-p hkey-value) => t))
    (should (equal (hkey-actions)
                   (cons '(ignore-errors (smart-prog-tag hkey-value))
			 '(ignore-errors (smart-prog-tag hkey-value))))))

  ;; Python files
  (let ((major-mode 'python-mode))
    (mocklet (((hypb:buffer-file-name) => "buffer")
              ((smart-python-at-tag-p) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-python hkey-value) '(smart-python hkey-value 'next-tag))))))
  (let ((major-mode 'org-mode))
    (mocklet (((hsys-org-get-value :language) => "python")
              ((smart-python-at-tag-p) => t)
              ;; !!FIXME - Needed to block smart-org from triggering
              ;; !!the smart-org rule. BUG?
              ((smart-org) => nil))
      (should (equal (hkey-actions)
                     (cons '(smart-python hkey-value) '(smart-python hkey-value 'next-tag))))))
  (let ((major-mode 'java-mode))
    (cl-letf (((symbol-function 'buffer-name) (lambda (&optional _) "Python")))
      (mocklet (((smart-python-at-tag-p) => t))
        (should (equal (hkey-actions)
                       (cons '(smart-python hkey-value) '(smart-python hkey-value 'next-tag)))))))

  ;; c-mode
  (let ((major-mode 'c-mode))
    (mocklet (((hypb:buffer-file-name) => "buffer-file-name")
              ((smart-c-at-tag-p) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-c) '(smart-c nil 'next-tag))))))
  ;; c++-mode
  (let ((major-mode 'c++-mode))
    (mocklet (((hypb:buffer-file-name) => "buffer-file-name")
              ((smart-c-at-tag-p) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-c++) '(smart-c++ nil 'next-tag))))))

  ;; asm-mode
  (let ((major-mode 'asm-mode))
    (mocklet (((hypb:buffer-file-name) => "buffer-file-name")
              ((smart-asm-at-tag-p) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-asm) '(smart-asm nil 'next-tag))))))

  ;; smart-lisp
  (mocklet (((smart-lisp-mode-p) => t))
    (mocklet (((smart-lisp-at-load-expression-p) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-lisp) '(smart-lisp 'show-doc)))))
    (mocklet (((smart-lisp-at-load-expression-p) => nil)
              ((smart-lisp-at-tag-p) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-lisp) '(smart-lisp 'show-doc))))))
  ;;; !!FIXME(BUG!?) -- See source: "../hui-mouse.el:L470"
  ;; When smart-lisp-mode-p is t then hkey-value can be set
  ;; non-nil. That will however make the predicate complete non-nil
  ;; and smart-lisp-at-change-log-tag-p will never be called!? Should
  ;; maybe the `or' statement be an `and' so that
  ;; smart-lisp-at-change-log-tag-p cabn be called in case
  ;; smart-lisp-at-tag-p is nil!?
  ;;;
  ;; (mocklet (((smart-lisp-mode-p) => nil) ((smart-lisp-at-change-log-tag-p) => t))
  ;;   (should (equal (hkey-actions)
  ;;                  (cons '(smart-prog-tag hkey-value) '(smart-prog-tag hkey-value)))))

  ;; Java
  (let ((major-mode 'java-mode))
    (mocklet (((hypb:buffer-file-name) => "buffer-file-name"))
      (mocklet (((smart-java-at-tag-p) => t))
        (should (equal (hkey-actions)
                       (cons '(smart-java) '(smart-java nil 'next-tag)))))
      (mocklet (((smart-java-at-tag-p) => nil))
        (cl-letf (((symbol-function 'looking-at)
                   (lambda (str &optional _) (string= str "@see[ \t]+"))))
          (should (equal (hkey-actions)
                         (cons '(smart-java) '(smart-java nil 'next-tag)))))
        (let ((looking-at-value nil))
          (cl-letf (((symbol-function 'looking-at) (lambda (str &optional _) looking-at-value))
                    ((symbol-function 're-search-backward)
                     (lambda (str &optional _ _ _) (if (string= str "[@\n\r\f]") (setq looking-at-value t)))))
            (should (equal (hkey-actions)
                           (cons '(smart-java) '(smart-java nil 'next-tag))))))
        )))

  ;; html-mode javascript-mode js-mode js-ts-mode js2-mode js3-mode web-mode
  (let ((major-mode 'html-mode))
    (mocklet (((hypb:buffer-file-name) => "buffer-file-name")
              ((smart-javascript-at-tag-p) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-javascript) '(smart-javascript nil 'next-tag))))))

  ;; objc-mode
  (let ((major-mode 'objc-mode))
    (mocklet (((hypb:buffer-file-name) => "buffer-file-name")
              ((smart-objc-at-tag-p) => t))
      (should (equal (hkey-actions)
                     (cons '(smart-objc) '(smart-objc nil 'next-tag))))))

  ;; Imenu listing
  (mocklet (((smart-imenu-item-at-p) => t))
    (should (equal (hkey-actions)
                   (cons '(smart-imenu-display-item-where (car hkey-value) (cdr hkey-value))
                         '(imenu-choose-buffer-index)))))

  ;; fortran-mode f90-mode
  (dolist (m '(fortran-mode f90-mode))
    (let ((major-mode m))
      (mocklet (((hypb:buffer-file-name) => "buffer-file-name")
                ((smart-fortran-at-tag-p) => t))
        (should (equal (hkey-actions)
                       (cons '(smart-fortran) '(smart-fortran nil 'next-tag)))))))

  ;; hmail
  (defvar hmail:reader)
  (defvar hmail:lister)
  (let ((hmail:reader 'reader)
        (hmail:lister 'reader))
    (dolist (m (list hmail:reader hmail:lister))
      (let ((major-mode m))
        (should (equal (hkey-actions)
                       (cons '(smart-hmail) '(smart-hmail-assist)))))))

  ;; Follow references in man pages.
  (mocklet (((smart-man-entry-ref) => 'man-entry-ref))
    (should (equal (hkey-actions)
                   (cons '(smart-man-display hkey-value) '(smart-man-display hkey-value)))))

  ;; OO-Browser
  (mocklet (((br-in-browser) => t))
    (should (equal (hkey-actions)
                   (cons '(smart-br-dispatch) '(smart-br-assist-dispatch)))))

  ;; Outline minor mode
  (let ((outline-minor-mode t))
    (should (equal (hkey-actions)
                   (cons '(smart-outline) '(smart-outline-assist)))))

  ;;; No action matches
  (mocklet (((smart-prog-at-tag-p) => nil))
    (should-not (hkey-actions))))

(provide 'hui-mouse-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hui-mouse-tests.el ends here
