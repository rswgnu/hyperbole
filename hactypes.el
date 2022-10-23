;;; hactypes.el --- Default action types for GNU Hyperbole  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    23-Sep-91 at 20:34:36
;; Last-Mod:     23-Jul-22 at 22:25:35 by Bob Weiner
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(bookmark hvar hsettings comint hbut hpath hargs hmail)))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function kotl-mode:goto-cell "kotl-mode")
(declare-function kotl-mode:beginning-of-buffer "kotl-mode")
(declare-function rmail:msg-to-p "hrmail")

;;; ************************************************************************
;;; Standard Hyperbole action types
;;; ************************************************************************

(defact annot-bib (key)
  "Follow internal ref KEY within an annotated bibliography, delimiters=[]."
  (interactive "sReference key (no []): ")
  (let ((key-regexp (concat "^[*]*[ \t]*\\[" (ebut:key-to-label key) "\\]"))
	citation)
    (if (save-excursion
	  (goto-char (point-max))
	  (setq citation (re-search-backward key-regexp nil t)))
	(progn (hpath:display-buffer (current-buffer))
	       (goto-char citation)
	       (beginning-of-line))
      (beep))))

(defact completion ()
  "Insert completion at point into the minibuffer or a buffer.
Unless point is at the end of the buffer or if a completion has already been
inserted, delete the completions window."
  (interactive)
  (if (smart-eobp)
      (progn (bury-buffer nil)
	     (delete-window))
    (hargs:completion)))

(defact display-boolean (bool-expr)
  "Display a message showing the result value of a BOOL-EXPR.
Return any non-nil value or t."
  (let ((result (eval bool-expr t)))
    (message "Boolean result (%s) = %S; Expr: %S"
	     (if result "True" "False") result bool-expr)
    (or result t)))

(defact display-value (value)
  "Display a message showing VALUE (a symbol) and its value.
Return any non-nil value or t."
  (let ((result (eval value)))
    (message "%S" result)
    (or result t)))

(defact display-variable (var)
  "Display a message showing VAR (a symbol) and its value.
Return any non-nil value or t."
  (message "%s = %S" var (symbol-value var))
  (or (symbol-value var) t))

(defact eval-elisp (lisp-expr)
  "Evaluate a LISP-EXPR for its side-effects and return any non-nil value."
  (interactive "xLisp to eval: ")
  (eval lisp-expr t))

(defact exec-kbd-macro (kbd-macro &optional repeat-count)
  "Execute KBD-MACRO REPEAT-COUNT times.
KBD-MACRO may be a string of editor command characters, a function symbol or
nil to use the last defined keyboard macro.
Optional REPEAT-COUNT nil means execute once, zero means repeat until
error."
  (interactive
   (let (macro repeat)
     (setq macro (intern-soft
		  (hargs:read-match
		   "Unquoted macro name or nil for last one defined: "
		   obarray (lambda (sym)
			     (and (fboundp sym)
				  (stringp (hypb:indirect-function sym))))
		   nil "nil" 'symbol)))
     (cond ((fboundp macro))
	   ((null last-kbd-macro)
	    (hypb:error
	     "(exec-kbd-macro): Define a keyboard macro first"))
	   (t (defalias '$%macro last-kbd-macro)
	      (setq macro '$%macro)))
     (save-excursion
       (let ((standard-output (get-buffer-create "*macro-def*")))
	 (unwind-protect
	     (progn (set-buffer standard-output)
		    (setq buffer-read-only nil)
		    (erase-buffer)
		    (insert-kbd-macro macro)
		    (goto-char (point-min))
		    (setq macro (car (cdr (cdr (read (current-buffer)))))))
	   (kill-buffer standard-output))))
     (fmakunbound '$%macro)
     (setq repeat (hargs:read "Repeat count: "
			      (lambda (repeat)
				(or (null repeat)
				    (and (integerp repeat) (>= repeat 0))))
			      1))
     (list macro repeat)))
  (unless (called-interactively-p 'interactive)
    (or (and kbd-macro (or (stringp kbd-macro)
			   (and (symbolp kbd-macro) (fboundp kbd-macro))))
	(hypb:error "(exec-kbd-macro): Bad macro: %s" kbd-macro))
    (or (null repeat-count) (and (integerp repeat-count) (<= 0 repeat-count))
	(hypb:error "(exec-kbd-macro): Bad repeat count: %s" repeat-count)))
  (execute-kbd-macro kbd-macro repeat-count))

;;; Support next two actypes on systems which use the `comint' shell package.
(defact exec-shell-cmd (shell-cmd &optional internal-cmd kill-prev)
  "Execute a SHELL-CMD string asynchronously.
Optional non-nil second argument INTERNAL-CMD inhibits display of the shell
command line executed.  Optional non-nil third argument KILL-PREV means
kill the last output to the shell buffer before executing SHELL-CMD."
  (interactive
   (let ((default  (car hargs:defaults))
	 (default1 (nth 1 hargs:defaults))
	 (default2 (nth 2 hargs:defaults)))
     (list (hargs:read "Shell cmd: "
		       (lambda (cmd) (not (string-equal cmd "")))
		       default "Enter a shell command." 'string)
	   (y-or-n-p (format "Omit cmd from output (default = %s)? "
			     default1))
	   (y-or-n-p (format "Kill prior cmd's output (default = %s)? "
			     default2)))))
  (require 'comint)
  (let ((buf-name "*Hyperbole Shell*")
	(obuf (current-buffer))
	(default-dir (expand-file-name default-directory)))
    (unwind-protect
	(progn
	  (unless (hpath:remote-p default-dir)
	    (setq shell-cmd
		  (concat "cd " default-dir " && " shell-cmd)))
	  (if (and (get-buffer buf-name)
		   (get-buffer-process (get-buffer buf-name)))
	      (hpath:display-buffer buf-name)
	    ;; (hpath:display-buffer (current-buffer))
	    (when (eq (minibuffer-window) (selected-window))
	      (other-window 1))
	    ;; 'shell' calls pop-to-buffer which normally displays in
	    ;; another window
	    (setq buf-name (buffer-name (shell buf-name))))
	  (while (not (and (buffer-live-p (get-buffer buf-name))
			   (buffer-modified-p (get-buffer buf-name))))
	    ;; Wait for shell to startup before sending it input.
	    (sit-for 1))
	  (setq comint-last-input-start (point-marker)
		comint-last-input-end (point-marker)))
      (goto-char (point-max))
      (and kill-prev comint-last-input-end
	   (not (equal comint-last-input-start comint-last-input-end))
	   (comint-delete-output))
      (insert shell-cmd)
      (comint-send-input)
      (comint-show-output)
      (or internal-cmd (scroll-down 1)))
    (select-window (or (get-buffer-window obuf t) (selected-window)))))

(defact exec-window-cmd (shell-cmd)
  "Asynchronously execute an external window-based SHELL-CMD string."
  (interactive
   (let ((default  (car hargs:defaults)))
     (list (hargs:read "Shell cmd: "
		       (lambda (cmd) (not (string-equal cmd "")))
		       default "Enter a shell command." 'string))))
  (require 'comint)
  (let* ((buf-name "*Hyperbole Shell*")
	 (default-dir (expand-file-name default-directory))
	 (cmd (if (hpath:remote-p default-dir)
		  (concat "(" shell-cmd ") &")
		(concat "(cd " default-dir " && " shell-cmd ") &")))
	 (msg (format "Executing: %s" shell-cmd)))
    (message msg)
    (save-excursion
      (save-window-excursion
	(unless (and (get-buffer buf-name)
		     (get-buffer-process (get-buffer buf-name)))
	  (save-excursion
	    (save-window-excursion
	      (setq buf-name (buffer-name (shell buf-name)))))
	  (message msg)
	  ;; Wait for shell to startup before sending it input.
	  (sit-for 1)
	  (set-buffer buf-name)
	  (setq comint-last-input-start (point-marker)
		comint-last-input-end (point-marker)))
	(unless (equal (buffer-name (current-buffer)) buf-name)
	  (set-buffer buf-name))
	(goto-char (point-max))
	(insert cmd)
	(comint-send-input)))
    (message msg)))

(defact hyp-config (&optional out-buf)
  "Insert Hyperbole configuration information at the end of the current buffer.
Insert in optional OUT-BUF if specified."
  (hypb:configuration out-buf))

(defact hyp-request (&optional out-buf)
  "Insert into optional OUT-BUF how to (un)subscribe from a Hyperbole mail list."
  (save-excursion
    (and out-buf (set-buffer out-buf))
    ;; Allows for insertion prior to user's email signature
    (unless (search-forward "\n\n" nil t)
      (goto-char (point-max)))
    (delete-blank-lines) (delete-blank-lines)
    (insert "Use one of the following formats in the To: <email-address> of your message:\n
To join a list:                   <list-name>-join@gnu.org
To leave a list:                  <list-name>-leave@gnu.org
To contact the list maintainer:   <list-name>-owner@gnu.org
To change your address on a list: send a leave email, followed by a separate join email,

where possible <list-names> are:
  hyperbole-users    - Hyperbole discussion, questions and announcements
  bug-hyperbole      - Report Hyperbole problems, not for support requests .

For example:  To: hyperbole-users-join@gnu.org\n")))

(defact hyp-source (buf-str-or-file)
  "Display a buffer or file from a line beginning with `hbut:source-prefix'."
  (interactive
   (list (prin1-to-string (get-buffer-create
			   (read-buffer "Buffer to link to: "))
			  t)))
  (if (stringp buf-str-or-file)
      (cond ((string-match "\\`#<buffer \"?\\([^ \n\"]+\\)\"?>" buf-str-or-file)
	     (hpath:display-buffer
	      (substring buf-str-or-file (match-beginning 1) (match-end 1))))
	    (t (hpath:find buf-str-or-file)))
    (hypb:error "(hyp-source): Non-string argument: %s" buf-str-or-file)))

(defact link-to-bookmark (bookmark)
  "Display an Emacs BOOKMARK (a name).
When creating the button, if in Bookmark Menu mode, use the bookmark
nearest point as the default.  Otherwise, utilize the most recently used
bookmark in the current file (bookmark-current-bookmark) as the default,
if any."
  (interactive
   (list (bookmark-completing-read "Bookmark to link to"
				   (if (derived-mode-p 'bookmark-bmenu-mode)
				       (bookmark-bmenu-bookmark)
				     bookmark-current-bookmark))))
  (bookmark-jump bookmark (hpath:display-buffer-function)))

(defact link-to-buffer-tmp (buffer &optional point)
  "Display a BUFFER scrolled to optional POINT.
If POINT is given, the buffer is displayed with POINT at the top of
the window.

This type of link is for use within a single editor session.  Use
`link-to-file' instead for a permanent link."
  (interactive "bBuffer to link to: ")
  (if (or (stringp buffer) (bufferp buffer))
      (and (hpath:display-buffer buffer)
	   (integerp point)
	   (progn (goto-char (min (point-max) point))
		  (recenter 0)))
    (hypb:error "(link-to-buffer-tmp): Not a current buffer: %s" buffer)))

(defact link-to-directory (directory)
  "Display a DIRECTORY in Dired mode."
  (interactive "DDirectory to link to: ")
  (hpath:find directory))

(defact link-to-ebut (key &optional key-file)
  "Perform explicit button action specified by KEY and optional KEY-FILE.
Interactively, KEY-FILE defaults to the current buffer's file name."
  (interactive
   (let (but-lbl
         but-file)
     (while (cond ((setq but-file
                         (read-file-name
                          "File of button to link to: " nil nil t))
                   (if (string-equal but-file "")
                       (progn (beep) t)))
                  ((not (file-readable-p but-file))
                   (message "(link-to-ebut): You cannot read `%s'."
                            but-file)
                   (beep) (sit-for 3))))
     (list (progn
             (find-file-noselect but-file)
             (while (string-equal "" (setq but-lbl
                                           (hargs:read-match
                                            "Button to link to: "
                                            (ebut:alist but-file)
                                            nil nil nil 'ebut)))
               (beep))
             (ebut:label-to-key but-lbl))
           but-file)))
  (let (but
        normalized-file)
    (if key-file
        (setq normalized-file (hpath:normalize key-file))
      (setq normalized-file buffer-file-name))

    (if (setq but (when normalized-file (ebut:get key nil normalized-file)))
        (hbut:act but)
      (hypb:error "(link-to-ebut): No button `%s' in `%s'"
                  (ebut:key-to-label key)
                  key-file))))

(defact link-to-elisp-doc (symbol)
  "Display documentation for SYMBOL."
  (interactive "SSymbol to display documentation for: ")
  (cond ((not (symbolp symbol))
	 (hypb:error "(link-to-elisp-doc): `%s' not a symbol" symbol))
	((not (or (boundp symbol) (fboundp symbol)))
	 (hypb:error "(link-to-elisp-doc): `%s' not defined" symbol))
	(t (let ((temp-buffer-show-function 'switch-to-buffer))
	     (hpath:display-buffer (help-buffer))
	     (describe-symbol symbol)))))

(defun  hactypes:link-to-file-interactively ()
  "Get a path to link to and return it as a one item list."
  (let ((prev-reading-p hargs:reading-type)
	(existing-buf t)
	path-buf)
    (unwind-protect
	(let* ((default-directory (or (hattr:get 'hbut:current 'dir)
				      (file-name-directory
				       (or (hattr:get 'hbut:current 'loc) ""))
				      default-directory))
	       (file-path (or (car hargs:defaults) default-directory))
	       (file-point (cadr hargs:defaults))
	       (hargs:reading-type 'file)
	       ;; If reading interactive inputs from a key series
	       ;; (puts key events into the unread queue), then don't
	       ;; insert default-directory into the minibuffer
	       ;; prompt, allowing time to remove any extra pathname
	       ;; quotes added in the key series.
	       (insert-default-directory (not unread-command-events))
	       ;; Remove any double quotes and whitespace at the
	       ;; start and end of the path that interactive use may
	       ;; have introduced.
	       (path (hpath:trim (read-file-name "Path to link to: "
						 file-path file-path)))
	       (orig-path path)
	       path-line-and-col
	       line-num
	       column-num
	       normalized-path)
	  ;; Handle if :line:column are included in path.
	  (setq path-line-and-col (hpath:file-line-and-column path))
	  (when path-line-and-col
	    (setq path (nth 0 path-line-and-col)
		  line-num (nth 1 path-line-and-col)
		  column-num (nth 2 path-line-and-col)))
	  ;; Ensure any variables and heading suffixes following
	  ;; [#,] are removed before doing path matching.
	  (setq normalized-path (or (hpath:is-p path) path))
	  (when (not (or (file-name-absolute-p path)
			 (string-match "\\`\\$\{" path)))
	    (setq path (concat default-directory path)))
	  (setq existing-buf (get-file-buffer normalized-path)
		path-buf (or existing-buf
			     (and (file-readable-p normalized-path)
				  (prog1 (set-buffer (find-file-noselect normalized-path t))
				    (when (integerp file-point)
				      (goto-char (min (point-max) file-point)))))))
	  (if (and path-buf (not line-num))
	      (with-current-buffer path-buf
		(setq hargs:reading-type 'character)
		(if (y-or-n-p
		     (format "y = Display at present position (line %d); n = no position? "
			     (count-lines 1 (point))))
		    (list path (point))
		  (list path)))
	    (if path-buf
		(delq nil (list path (save-excursion
				       (goto-char (point-min))
				       (forward-line (1- line-num))
				       (when column-num (move-to-column column-num))
				       (point))))
	      (list (or path orig-path)))))
      (setq hargs:reading-type prev-reading-p)
      (when (and path-buf (not existing-buf))
	(kill-buffer path-buf)))))

(defact link-to-file (path &optional point)
  "Display a file given by PATH scrolled to optional POINT.
If POINT is given, display the buffer with POINT at the top of
the window or as close as possible."
  (interactive (hactypes:link-to-file-interactively))
  (if path
      (progn
	;; Remove any double quotes and whitespace at the start and end of
	;; the path that use within a key series may have introduced.
	(setq path (hpath:trim path))
	(and (hpath:find path)
	     (integerp point)
	     (progn (goto-char (min (point-max) point))
		    (recenter 0))))
    (hypb:error "(link-to-file): Invalid file name: \"%s\"" path)))

(defact link-to-file-line (path line-num)
  "Display a file given by PATH scrolled to LINE-NUM."
  (interactive "fPath to link to: \nnDisplay at line number: ")
  ;; Remove any double quotes and whitespace at the start and end of
  ;; the path that interactive use may have introduced.
  (setq path (hpath:trim path))
  (if (condition-case ()
	  (setq path (smart-tags-file-path path))
	(error t))
      (if (and (stringp path) (not (file-name-absolute-p path))
	       (compilation-buffer-p (current-buffer)))
	  (compile-goto-error)
	(hpath:find-line path line-num))))

(defact link-to-file-line-and-column (path line-num column-num)
  "Display a file given by PATH scrolled to LINE-NUM with point at COLUMN-NUM."
  (interactive "fPath to link to: \nnDisplay at line number: \nnand column number: ")
  ;; Remove any double quotes and whitespace at the start and end of
  ;; the path that interactive use may have introduced.
  (setq path (hpath:trim path))
  (when (condition-case ()
	    (setq path (smart-tags-file-path path))
	  (error t))
    (if (and (stringp path) (not (file-name-absolute-p path))
	     (compilation-buffer-p (current-buffer)))
	(compile-goto-error)
      (hpath:find-line path line-num))
    (move-to-column column-num)))

(defact link-to-gbut (key &optional _key-file)
  "Perform an action given by an existing global button, specified by KEY.
Optional second arg, KEY-FILE, is not used but is for calling
compatibility with the `hlink' function."
  (interactive
   (let ((gbut-file (hpath:validate (hpath:substitute-value (gbut:file))))
	 but-lbl)
     (if (not (file-readable-p gbut-file))
	 (hypb:error "(link-to-gbut): You cannot read `%s'" gbut-file)
       (list (progn
	       (find-file-noselect gbut-file)
	       (while (string-equal "" (setq but-lbl
					     (hargs:read-match
					      "Global button to link to: "
					      (mapcar #'list (gbut:label-list))
					      nil t nil 'gbut)))
		 (beep))
	       (hbut:label-to-key but-lbl))))))
  (gbut:act (hbut:key-to-label key)))

(defact link-to-Info-index-item (index-item)
  "Display an Info index INDEX-ITEM cross-reference.
INDEX-ITEM must be a string of the form \"(filename)item-name\".
During button creation, completion for both filename and
item-name is available.  Filename may be given without the .info
suffix."
  (interactive "+XInfo (file)index-item-name to link to: ")
  (require 'info)
  (when (stringp index-item)
    ;; Remove any tabs or newlines that might be in index-item.
    (setq index-item (replace-regexp-in-string "[ \t\n\r\f]+" " " index-item t t)))
  (if (and (stringp index-item) (string-match "^(\\([^\)]+\\))\\(.*\\)" index-item))
      (id-info-item index-item)
    (hypb:error "(link-to-Info-index-entry): Invalid Info index item: `%s'" index-item)))

(defact link-to-Info-node (string)
  "Display an Info node given by STRING.
If not found, try to display it as an Info index item.
STRING must be a string of the form \"(filename)name\".  During
button creation, completion for both filename and node names is
available.  Filename may be given without the .info suffix."
  (interactive "+IInfo (file)nodename to link to: ")
  (require 'info)
  (when (stringp string)
    ;; Remove any tabs or newlines that might be in string.
    (setq string (replace-regexp-in-string "[ \t\n\r\f]+" " " string t t)))
  (if (and (stringp string) (string-match "^(\\([^\)]+\\))\\(.*\\)" string))
      (id-info string)
    (hypb:error "(link-to-Info-node): Invalid Info node: `%s'" string)))

(defact link-to-ibut (key &optional but-src point)
  "Perform implicit button action specified by KEY, optional BUT-SRC and POINT.
BUT-SRC defaults to the current buffer's file or if there is no
attached file, then to its buffer name.  POINT defaults to the
current point.

When the button with this action type is created, point must be
on the implicit button to which to link."
  (interactive
   (let ((ibut-key (ibut:at-p t)))
     (cond (ibut-key
	    (list ibut-key (or buffer-file-name (buffer-name)) (point)))
	   ;; TODO: If default is null below and are creating, rather than editing
	   ;; the link, it would be better to throw an error than create
	   ;; an invalid link, but it is difficult to tell which operation
	   ;; is in progress, so ignore this for now.  -- RSW, 01-25-2020

	   ;; When not on an ibut and editing the link, use existing arguments
	   ((and (bound-and-true-p hargs:defaults) (listp hargs:defaults) hargs:defaults)
	    hargs:defaults)
	   (t
	    (hypb:error "(link-to-ibut): Point must be on an implicit button to create a link-to-ibut")))))
  (when (null key)
    (hypb:error "(link-to-ibut): Point must be on an implicit button to create a link-to-ibut"))
  (let (but
	normalized-file)
    (cond (but-src
	   (unless (and (get-buffer but-src)
			(not (buffer-file-name (get-buffer but-src))))
	     (setq normalized-file (hpath:normalize but-src))))
	  (t (setq normalized-file buffer-file-name)))
    (save-excursion
      (save-restriction
	(when but-src
	  (set-buffer (or (get-buffer but-src) (get-file-buffer normalized-file))))
	(widen)
	(when (or (not normalized-file) (hmail:editor-p) (hmail:reader-p))
	  (hmail:msg-narrow))
	(when (integerp point)
	  (goto-char (min point (point-max))))
	(setq but (ibut:to key))))
    (cond (but
	   (hbut:act but))
	  (key
	   (hypb:error "(link-to-ibut): No implicit button `%s' found in `%s'"
		       (ibut:key-to-label key)
		       (or but-src (buffer-name))))
	  (t
	   (hypb:error "(link-to-ibut): Link reference is null/empty")))))

(defact link-to-kcell (file cell-ref)
  "Display FILE with kcell given by CELL-REF at window top.
See documentation for `kcell:ref-to-id' for valid cell-ref formats.

If FILE is nil, use the current buffer.
If CELL-REF is nil, show the first cell in the view."
  (interactive "fKotl file to link to: \n+KKcell to link to: ")
  (require 'kfile)
  (cond	((if file
	     (hpath:find file)
	   (hpath:display-buffer (current-buffer)))
	 (if cell-ref
	     (kotl-mode:goto-cell cell-ref)
	   (kotl-mode:beginning-of-buffer))
	 (recenter 0))
	((and (stringp cell-ref) (> (length cell-ref) 0)
	      (eq ?| (aref cell-ref 0)))
	 ;; Activate view spec in current window.
	 (kotl-mode:goto-cell cell-ref))))

(defact link-to-mail (mail-msg-id &optional mail-file)
  "Display mail msg with MAIL-MSG-ID from optional MAIL-FILE.
See documentation for the variable `hmail:init-function' for
information on how to specify a mail reader to use."
  (interactive "+MMail Msg: ")
  (if (not (fboundp 'rmail:msg-to-p))
      (hypb:error "(link-to-mail): Invoke mail reader before trying to follow a mail link")
    (if (and (listp mail-msg-id) (null mail-file))
	(setq mail-file (car (cdr mail-msg-id))
	      mail-msg-id (car mail-msg-id)))
    (let ((wconfig (current-window-configuration)))
      (hpath:display-buffer (current-buffer))
      ;; Initialize user-specified mail reader if need be.
      (if (and (symbolp hmail:init-function)
	       (fboundp hmail:init-function)
	       (listp (symbol-function hmail:init-function))
	       (eq 'autoload (car (symbol-function hmail:init-function))))
	  (funcall hmail:init-function))
      (unless (rmail:msg-to-p mail-msg-id mail-file)
	;; Couldn't find message, restore old window config, report error
	(set-window-configuration wconfig)
	(hypb:error "(link-to-mail): No msg `%s' in file \"%s\""
		    mail-msg-id mail-file)))))

(defact link-to-regexp-match (regexp n source &optional buffer-p)
  "Find REGEXP's Nth occurrence in SOURCE and display location at window top.
SOURCE is a pathname unless optional BUFFER-P is non-nil, then SOURCE must be
a buffer name or buffer.
Return t if found, signal an error if not."
  (interactive "sRegexp to match: \nnOccurrence number: \nfFile to search: ")
  (let ((orig-src source))
    (if buffer-p
	(if (stringp source)
	    (setq source (get-buffer source)))
      ;; Source is a pathname.
      (if (not (stringp source))
	  (hypb:error
	   "(link-to-regexp-match): Source parameter is not a filename: `%s'"
	   orig-src)
	(setq source (find-file-noselect (hpath:substitute-value source)))))
    (if (not (bufferp source))
	(hypb:error
	 "(link-to-regexp-match): Invalid source parameter: `%s'" orig-src)
      (hpath:display-buffer source)
      (widen)
      (goto-char (point-min))
      (if (re-search-forward regexp nil t n)
	  (progn (beginning-of-line) (recenter 0) t)
	(hypb:error
	 "(link-to-regexp-match): Pattern not found: `%s'" regexp)))))

(defact link-to-rfc (rfc-num)
  "Retrieve and display an Internet rfc given by RFC-NUM.
RFC-NUM may be a string or an integer."
  (interactive "nRFC number to retrieve: ")
  (when (or (stringp rfc-num) (integerp rfc-num))
    (browse-url-emacs (hpath:rfc rfc-num))))

(defact link-to-string-match (string n source &optional buffer-p)
  "Find STRING's Nth occurrence in SOURCE and display location at window top.
SOURCE is a pathname unless optional BUFFER-P is non-nil, then SOURCE must be
a buffer name or buffer.
Return t if found, nil if not."
  (interactive "sString to match: \nnOccurrence number: \nfFile to search: ")
  (funcall (actype:action 'link-to-regexp-match)
	   (regexp-quote string) n source buffer-p))

(defact link-to-texinfo-node (file node)
  "Display the Texinfo FILE and NODE (a string).
FILE may be a string or nil, in which case the current buffer is used."
  (interactive "fTexinfo file to link to: \nsNode within file to link to: ")
  (when (stringp node)
    ;; Remove any tabs or newlines that might be in node name.
    (setq node (replace-regexp-in-string "[ \t\n\r\f]+" " " node t t)))
  (let (node-point)
    (if file
        (set-buffer (find-file-noselect (hpath:substitute-value file)))
      (setq file buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (format "^@node[ \t]+%s *[,\n\r]" node) nil t)
	  (setq node-point (match-beginning 0))
	(hypb:error "(link-to-texinfo-node): Non-existent node: \"%s%s\""
                    (if file
                        (format "(%s)" (file-name-nondirectory file))
                      "")
		    node)))
    (if file
        (hact 'link-to-file file node-point)
      (hypb:error "(link-to-texinfo-node): Non-existent node: \"%s\""
		  node))))

(defact link-to-web-search (service-name search-term)
  "Search web SERVICE-NAME for SEARCH-TERM.
Uses `hyperbole-web-search-alist' to match each service to its search url.
Uses `hyperbole-web-search-browser-function' and the `browse-url'
package to display search results."
  (interactive (hyperbole-read-web-search-arguments))
  (hyperbole-web-search service-name search-term))

(defact man-show (topic)
  "Display man page on TOPIC, which may be of the form <command>(<section>).
Uses `hpath:display-where' setting to control where the man page is displayed."
  (interactive "sManual topic: ")
  (require 'man)
  (let ((Man-notify-method 'meek))
    (hpath:display-buffer (man topic))))

(defact rfc-toc (&optional buf-name opoint sections-start)
  "Compute and display summary of an Internet rfc in BUF-NAME.
Assume point has already been moved to start of region to summarize.
Optional OPOINT is point to return to in BUF-NAME after displaying summary."
  (interactive)
  (if buf-name
      (cond ((get-buffer buf-name)
	     (switch-to-buffer buf-name))
	    ((let ((buf (get-file-buffer buf-name)))
	       (when buf
		 (switch-to-buffer (setq buf-name buf))
		 t)))
	    (t (if opoint (goto-char opoint))
	       (hypb:error "(rfc-toc): Invalid buffer name: %s" buf-name))))
  (let ((sect-regexp "^[ \t]*[1-9][0-9]*\\.[0-9.]*[ \t]+[^ \t\n\r]")
	(temp-buffer-show-function 'switch-to-buffer)
	occur-buffer)
    (hpath:display-buffer (current-buffer))
    (occur sect-regexp nil (list (cons sections-start (point-max))))
    (setq occur-buffer (set-buffer "*Occur*"))
    (rename-buffer (format "*%s toc*" buf-name))
    (re-search-forward "^[ ]*[0-9]+:" nil t)
    (beginning-of-line)
    (delete-region (point-min) (point))
    (insert "Contents of " (buffer-name occur-buffer) ":\n")
    (set-buffer-modified-p nil)
    (set-buffer buf-name)
    (if opoint (goto-char opoint))))

(defact text-toc (section)
  "Jump to the text file SECTION referenced by a table of contents entry at point."
  (interactive "sGo to section named: ")
  (if (stringp section)
      (progn
	(actypes::link-to-regexp-match
	 (concat "^\\*+[ \t]*" (regexp-quote section))
	 1 (current-buffer) t)
	(while (and (= (forward-line -1) 0)
		    (looking-at "[ \t]*[-=][-=]")))
	(forward-line 1)
	(recenter 0))))

(provide 'hactypes)

;;; hactypes.el ends here

