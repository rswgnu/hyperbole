;;; hargs.el --- GNU Hyperbole user input functions    -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    31-Oct-91 at 23:17:35
;; Last-Mod:     27-Feb-25 at 21:21:40 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1991-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This module should be used for any interactive prompting and
;;   argument reading that Hyperbole does through Emacs.
;;
;;   `hargs:iform-read' provides a complete Lisp-based replacement for
;;   interactive argument reading (most of what `call-interactively' does).
;;   It also supports prompting for new argument values with defaults drawn
;;   from current button arguments.  A few extensions to interactive argument
;;   types are also provided, see `hargs:iforms-extensions' for details.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'etags)                        ; For `find-tag--default'
(require 'hpath)
(require 'hypb)
;; Avoid any potential library name conflict by giving the load directory.
(require 'set (expand-file-name "set" hyperb:dir))
(require 'info)
(require 'hmouse-drv) ;; loads hui-mouse and hmouse-key

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function ivy-dispatching-done "ext:ivy")
(declare-function ivy-done "ext:ivy")
(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--command-p "ext:vertico")
(declare-function vertico--goto "ext:vertico")
(declare-function vertico--update "ext:vertico")
(declare-function vertico-exit "ext:vertico")
(declare-function vertico-insert "ext:vertico")
(declare-function vertico-mouse--index "ext:vertico")
(declare-function vertico--match-p "ext:vertico")

(declare-function ebut:label-p "hbut")
(declare-function gbut:file "hbut")
(declare-function hattr:get "hbut")
(declare-function hbut:label-p "hbut")
(declare-function hbut:label-to-key "hbut")
(declare-function hmail:reader-p "hmail")
(declare-function hui:menu-enter "hui")
(declare-function ibut:label-p "hbut")
(declare-function kbd-key:normalize "hib-kbd")
(declare-function kcell-view:label "kview")
(declare-function kcell-view:reference "kview")
(declare-function rmail:msg-id-get "hmail")

(declare-function smart-dired-pathname-up-to-point "hui-mouse")

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hargs:defaults nil
  "Default arguments read from an existing Hyperbole button when editing it.")

(defvar hargs:reading-type nil
  "Symbol representing the type of object Hyperbole is prompting the user to input.")

(add-hook 'completion-setup-hook #'hargs:set-string-to-complete)
(add-hook 'minibuffer-exit-hook  #'hargs:unset-string-to-complete)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hargs:reading-symbol nil
  "Remember what symbol is being read.")

(defvar hargs:string-to-complete nil
  "Minibuffer content the last time a completions buffer was generated, or nil.")

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defalias 'hargs:find-tag-default #'find-tag--default)

(defun hargs:action-get (action editing-flag)
  "Interactively get list of arguments for ACTION's parameters.
Current button is being edited when EDITING-FLAG is t.
Return nil if ACTION is not a list or `byte-code' object, has no
interactive form or takes no arguments."
  (save-excursion
    (and (or (subrp action) (byte-code-function-p action) (listp action)
             (and (fboundp 'closurep) (closurep action)))
	 (let ((interactive-form (action:commandp action)))
	   (when interactive-form
	     (hpath:relative-arguments
	      (hargs:iform-read interactive-form editing-flag)))))))

(defun hargs:buffer-substring (start end)
  "Return the buffer substring sans any properties between START and END positions.
Convert NUL characters to colons for use with grep lines."
  (let ((string (buffer-substring-no-properties start end)))
    ;; This may trigger on a colored grep-like output line which has
    ;; an embedded null character with a display text property that
    ;; displays it as a colon.  Since the display property is stripped
    ;; here, convert the null character to a colon.
    (subst-char-in-string ?\^@ ?: string t)))

(defun hargs:delimited (start-delim end-delim
			&optional start-regexp-flag end-regexp-flag
			list-positions-flag exclude-regexp as-key)
  "Return a delimited string that point is within the first line of, or nil.
The string matched may be up to two lines long.  The delimiters
are removed, the string is normalized and reduced to a single
line.  START-DELIM and END-DELIM are strings that specify the
argument delimiters.  With optional START-REGEXP-FLAG non-nil,
START-DELIM is treated as a regular expression.  END-REGEXP-FLAG
is similar.  With optional LIST-POSITIONS-FLAG, return list
of (string-matched start-pos end-pos).  Optional
EXCLUDE-REGEXP is compared against the match string with its delimiters
included; any string that matches this regexp is ignored.  With optional
AS-KEY = \\='none, return t rather than the string result.  Any other
non-nil value, means return the string normalized as a Hyperbole
button key (no spaces)." 
  (let* ((opoint (point))
	 ;; This initial limit is the forward search limit for start delimiters
	 (limit (if start-regexp-flag
		    opoint
		  ;; (min (+ opoint (1- (length start-delim)))
		  (min (+ opoint (length start-delim))
		       (point-max))))
	 (start-search-func (if start-regexp-flag 're-search-forward 'search-forward))
	 (end-search-func   (if end-regexp-flag   're-search-forward 'search-forward))
	 (count 0)
	 first
	 start ;; excludes delimiter
	 end   ;; excludes delimiter
	 start-pos
	 end-pos
	 start-with-delim
	 end-with-delim)

    (if (string-equal start-delim end-delim)
	(save-excursion
	  (beginning-of-line)
	  (while (and (setq end-pos (funcall start-search-func start-delim limit t))
		      (setq start-with-delim (match-beginning 0))
		      ;; Prevent infinite loop where regexp match does not
		      ;; move end-pos forward, e.g. match to bol.
		      (not (eq first end-pos))
		      (setq start end-pos)
		      (setq count (1+ count))
		      (< (point) opoint)
		      ;; This is not to find the real end delimiter but to find
		      ;; end delimiters that precede the current argument and are
		      ;; therefore false matches, hence the search is limited to
		      ;; prior to the original point.
		      (funcall end-search-func end-delim opoint t)
		      (setq count (1+ count)))
	    (setq first (or first start)
		  start nil))
	  (when (and (not start) (> count 0) (zerop (% count 2)))
	    ;; Since strings can span lines but this function matches only
	    ;; strings that start on the current line, when start-delim and
	    ;; end-delim are the same and there are an even number of
	    ;; delimiters in the search range, causing the end-delim
	    ;; search to match to what should probably be the start-delim,
	    ;; assume point is within a string and not between two other strings.
	    ;; -- RSW, 02-05-2019
	    (setq start (if (string-equal start-delim end-delim)
			    (point)
			  first))))
      ;;
      ;; Start and end delims are different, so don't have to worry
      ;; about whether in or outside two of the same delimiters and
      ;; can match much more simply.
      ;; Use forward rather than reverse search here to perform greedy
      ;; searches when optional matches within a regexp.
      (save-excursion
	(beginning-of-line)
	(while (and (<= (point) limit)
		    (setq start-pos (point)
			  end-pos (funcall start-search-func start-delim limit t))
		    ;; Prevent infinite loop where regexp match does not
		    ;; move end-pos forward, e.g. match to bol.
		    (not (eq start end-pos)))
	  (setq start-with-delim (match-beginning 0)
		start (match-end 0))
	  (when (eq start-pos end-pos)
	    ;; start-delim contains a match for bol, so move point
	    ;; forward a char to prevent loop exit even though start
	    ;; delim matched.
	    (goto-char (min (1+ (point)) (point-max)))))))

    (when start
      (save-excursion
	(forward-line 2)
	(setq limit (point))
	(goto-char opoint)
	(and (funcall end-search-func end-delim limit t)
	     (setq end (match-beginning 0)
		   end-with-delim (match-end 0)))))

    (when (and start end)
      (save-excursion
	;; Ignore any preceding backslash, e.g. when a double-quoted
	;; string is embedded within a doc string, except when
	;; the string starts with 2 backslashes or an MSWindows
	;; disk drive prefix, in which case the backslash is
	;; considered part of a pathname.
	(and (if (and (> end (point-min))
		      (= (or (char-before end) 0) ?\\)
		      (not (string-match (concat "\\(\\`[\\][\\]\\)\\|"
						 hpath:mswindows-mount-prefix)
					 (hargs:buffer-substring start end))))
		 (setq end (1- end))
	       t)
	     (< start end)
	     (>= end opoint)
	     (if (eq as-key 'none)
		 (if list-positions-flag
		     (list t start end)
		   t)
	       (let ((result (hargs:buffer-substring start end))
		     (string-with-delims (when (stringp exclude-regexp)
					   (hargs:buffer-substring start-with-delim
								   end-with-delim))))
		 (unless (and string-with-delims
			      (string-match exclude-regexp string-with-delims))
		   ;; Normalize the result
		   (setq result
			 (if as-key
			     (hbut:label-to-key result)
			   (replace-regexp-in-string "[\n\r\f]\\s-*"
						     " " result nil t)))
		   (unless hyperb:microsoft-os-p
		     (setq result (hpath:mswindows-to-posix result)))
		   (if list-positions-flag
		       (list result start end)
		     result)))))))))

(defun hargs:delimited-p (start-delim end-delim
			  &optional start-regexp-flag end-regexp-flag
			  list-positions-flag exclude-regexp)
  "Call `hargs:delimited' with its `as-key' arg set to `none'.
See `hargs:delimited' for full documentation."
  (hargs:delimited start-delim end-delim start-regexp-flag
		   end-regexp-flag list-positions-flag exclude-regexp 'none))

(defmacro hargs:make-iform-vector (&rest iform-alist)
  "Return a vector of interactive command code characters.
IFORM-ALIST is a list of elements of the form
    (INTERACTIVE-CMD-CHR  (ARGUMENT-TYPE . GET-ARGUMENT-FORM))
GET-ARGUMENT-FORM is executed in a context where it has access to
two variables `prompt' and `default'."
  ;; Vector needs to have 1 more elts than the highest char code for
  ;; interactive commands.
  (let ((size (1+ (car (sort (mapcar #'car iform-alist) #'>))))
        (vecsym (make-symbol "vec")))
    `(let ((,vecsym (make-vector ',size nil)))
       ,@(mapcar (lambda (elt)
		   `(aset ,vecsym ',(car elt)
		          (lambda (prompt default)
		            (ignore prompt default) ;; Don't warn if not used.
			    (let ((prev-reading-p hargs:reading-type))
			      (unwind-protect
				  (progn
				    ;; Use setq here to ensure change is
				    ;; visible in lexical subcontexts that are
				    ;; part of 'elt' body.
				    (setq hargs:reading-type ',(cadr elt))
				    ,(cddr elt))
				(setq hargs:reading-type prev-reading-p))))))
		 iform-alist)
       ,vecsym)))

(defconst hargs:iform-extensions-vector
  (hargs:make-iform-vector
   ;; Get existing Info node name, possibly prefixed with its (filename).
   (?I . (Info-node . (progn (require 'info)
			     ;; Prevent empty completions list from
			     ;; triggering an error in Info-read-node-name.
			     (unless (and Info-current-file-completions
					  (not (equal Info-current-file-completions '(("None")))))
			       (condition-case nil
				   (Info-build-node-completions)
				 (error (setq Info-current-file-completions '(("None"))))))
			     (Info-read-node-name prompt))))

   ;; Get kcell from some koutline.
   (?K . (kcell . (hargs:read prompt nil default nil 'kcell)))
   ;; Get kcell or path reference for use in a link.
   (?L . (klink . (hargs:read prompt nil default nil 'klink)))
   ;; Get existing mail msg date and file.
   (?M . (mail . (progn
		   (while
		       (or (not (listp
				 (setq default
				       (read-minibuffer
					(hargs:prompt
					 prompt ""
					 "list of (date mail-file)")
					default))))
			   (/= (length default) 2)
			   (not (and (stringp (car (cdr default)))
				     (file-exists-p
				      (car (cdr default))))))
		     (beep))
		   default)))
   ;; Get a Koutline viewspec.
   (?V . (kvspec . (hargs:read prompt nil nil nil 'kvspec)))
   ;; Get existing Info index item name, possibly prefixed with its (filename).
   (?X . (Info-index-item . (let (file item)
			      (require 'info)
			      (setq item (Info-read-index-item-name prompt))
			      (if (string-match "^(\\([^\)]+\\))\\(.*\\)" item)
			          item
			        (if (setq file (Info-current-filename-sans-extension))
			            (format "(%s)%s" file item)
			          item))))))
  "Vector of forms for each interactive command character code.")

(defconst hargs:iform-vector
  (hargs:make-iform-vector
   ;; Get function symbol.
   (?a . (symbol .
		 (intern (completing-read prompt obarray #'fboundp t default))))
   ;; Get name of existing buffer.
   (?b . (buffer .
		 (progn
		   (or default (setq default (other-buffer (current-buffer))))
		   (read-buffer prompt default t))))
   ;; Get name of possibly nonexistent buffer.
   (?B . (buffer .
		 (progn
		   (or default (setq default (other-buffer (current-buffer))))
		   (read-buffer prompt default nil))))
   ;; Get character.
   (?c . (character .
		    (progn (message
			    (if default
			        (hargs:prompt prompt
					      (if (integerp default)
					          (char-to-string default)
					        default)
					      "Curr:")
			      prompt))
			   (char-to-string (read-char)))))
   ;; Get symbol for interactive function, a command.
   (?C . (symbol .
		 (intern
		  (completing-read prompt obarray #'commandp t default))))
   ;; Get value of point; does not do I/O.
   (?d . (integer . (point)))
   ;; Get directory name.
   (?D . (directory .
		    (progn
		      (or default (setq default default-directory))
		      (read-directory-name prompt default default t))))
   ;; Get existing file name.
   (?f . (file .
	       (read-file-name prompt default default
			       (if (eq system-type 'vax-vms)
				   nil 'existing))))
   ;; Get possibly nonexistent file name.
   (?F . (file . (read-file-name prompt default default nil)))
   ;; Ignore this argument
   (?i . nil)
   ;; Get key sequence.
   (?k . (key .
	      (key-description (read-key-sequence
				(if default
				    (hargs:prompt prompt default "Curr:")
				  prompt)))))
   ;; Get key sequence without converting uppercase or shifted
   ;; function keys to their unshifted equivalents.
   (?K . (key .
	      (key-description (read-key-sequence
				(if default
				    (hargs:prompt prompt default "Curr:")
				  prompt)
				nil t))))
   ;; Get value of mark.  Does not do I/O.
   (?m . (integer . (marker-position (mark-marker))))
   ;; Get numeric prefix argument or a number from the minibuffer.
   (?N . (integer .
		  (if prefix-arg
		      (prefix-numeric-value prefix-arg)
		    (let ((arg))
		      (while (not (integerp
				   (setq arg (read-minibuffer prompt default))))
		        (beep))
		      arg))))
   ;; Get number from minibuffer.
   (?n . (integer .
		  (let ((arg))
		    (while (not (integerp
				 (setq arg (read-minibuffer prompt default))))
		      (beep))
		    arg)))
   ;; Get numeric prefix argument.  No I/O.
   (?p . (prefix-arg .
		     (prefix-numeric-value prefix-arg)))
   ;; Get prefix argument in raw form.  No I/O.
   (?P . (prefix-arg . prefix-arg))
   ;; Get region, point and mark as 2 args.  No I/O
   (?r . (region .
		 (if (marker-position (mark-marker))
		     (list 'args (min (point) (mark t))
			   (max (point) (mark t)))
		   (list 'args nil nil))))
   ;; Get string.
   (?s . (string . (read-string prompt default)))
   ;; Get symbol.
   (?S . (symbol .
		 (read-from-minibuffer
		  prompt default minibuffer-local-ns-map 'sym)))
   ;; Get variable name: symbol that is user-variable-p.
   (?v . (symbol . (read-variable
		    (if default
			(hargs:prompt prompt default "Curr:")
		      prompt))))
   ;; Get Lisp expression but don't evaluate.
   (?x . (sexpression . (read-minibuffer prompt default)))
   ;; Get Lisp expression and evaluate.
   (?X . (sexpression . (eval-minibuffer prompt default))))
  "Vector of forms for each interactive command character code.")

(defun hargs:get (interactive-entry &optional default prior-arg)
  "Prompt for an argument, if need be, from INTERACTIVE-ENTRY, a string.
Optional DEFAULT is inserted after prompt.
First character of INTERACTIVE-ENTRY must be a command character from
the list in the documentation for `interactive' or a `+' which
indicates that the following character is a Hyperbole interactive
extension command character.

May return a single value or a list of values, in which case the first
element of the list is always the symbol \\='args."
  (let (func cmd prompt)
    (cond ((or (null interactive-entry) (equal interactive-entry ""))
	   (error "(hargs:get): Empty interactive-entry arg"))
	  ;; Hyperbole / user extension command character.  The next
	  ;; character is the actual command character.
	  ((eq (aref interactive-entry 0) ?+)
	   (setq cmd (aref interactive-entry 1)
		 prompt (format (substring interactive-entry 2) prior-arg)
		 func (when (< cmd (length hargs:iform-extensions-vector))
			(aref hargs:iform-extensions-vector cmd)))
	   (if func
	       (funcall func prompt default)
	     (error
	      "(hargs:get): Bad interactive-entry extension character: `%c'"
	      cmd)))
	  ;; Normal interactive command character
	  (t (setq cmd (aref interactive-entry 0)
		   prompt
		   (format (substring interactive-entry 1) prior-arg)
		   func (when (< cmd (length hargs:iform-vector))
			  (aref hargs:iform-vector cmd)))
	     (if func
		 (funcall func prompt default)
	       (error
		"(hargs:get): Bad interactive-entry command character: `%c'"
		cmd))))))

;; Replicated from `vertico--match-p' in "vertico.el"
(defun hargs:match-p (str)
  "Return t if STR is a valid completion match."
  (let ((rm minibuffer--require-match))
    (or (memq rm '(nil confirm-after-completion))
        (equal "" str) ;; Null completion, returns default value
        (and (functionp rm) (funcall rm str)) ;; Emacs 29 supports functions
        (test-completion str minibuffer-completion-table minibuffer-completion-predicate)
        (if (eq rm 'confirm)
	    (eq (ignore-errors (read-char "Confirm")) 13)
          (minibuffer-message "Match required") nil))))

(defun hargs:prompt (prompt default &optional default-prompt)
  "Return string of PROMPT including DEFAULT.
Optional DEFAULT-PROMPT is used to describe default value."
  (if default
      (format "%s(%s%s%s) " prompt (or default-prompt "default")
	      (if (equal default "") "" " ")
	      default)
    prompt))

(defun hargs:select-event-window ()
  "Select window, if any, that mouse was over during last event."
  (let ((window (posn-window (event-start last-command-event))))
    (if (framep window)
	(setq window (frame-selected-window window)))
    (if (and (window-minibuffer-p window)
	     (not (minibuffer-window-active-p window)))
	(error "Attempt to select inactive minibuffer window")
      (select-window (or window (selected-window))))))

(defun hargs:set-string-to-complete ()
  "Store the current minibuffer contents into `hargs:string-to-complete'."
  (with-current-buffer (window-buffer (minibuffer-window))
    (setq hargs:string-to-complete (minibuffer-contents-no-properties))
    (when (equal hargs:string-to-complete "")
      (setq hargs:string-to-complete nil))))

(defun hargs:unset-string-to-complete ()
  "Remove any value from `hargs:string-to-complete'."
  (setq hargs:string-to-complete nil))

(defun hargs:sexpression-p (&optional no-recurse)
  "Return an sexpression at point as a string.
If point follows an sexpression end character, the preceding sexpression
is returned.  If point precedes an sexpression start character, the
following sexpression is returned.  Otherwise, the innermost sexpression
that point is within is returned or nil if none."
  (let ((not-quoted
	 '(condition-case ()
	      (not (and (= (if (char-after (- (point) 2))
			       (char-syntax (char-after (- (point) 2)))
			     0)
			   ?\\)
			(not (= (if (char-after (- (point) 3))
				    (char-syntax (char-after (- (point) 3)))
				  0)
				?\\))))
	    (error t))))
    (save-excursion
      (ignore-errors
	(cond ((and (eq (char-syntax (preceding-char)) ?\))
		    ;; Ignore quoted end chars.
		    (eval not-quoted))
	       (buffer-substring (point)
				 (progn (forward-sexp -1) (point))))
	      ((and (eq (char-syntax (following-char)) ?\()
		    ;; Ignore quoted begin chars.
		    (eval not-quoted))
	       (buffer-substring (point)
				 (progn (forward-sexp) (point))))
	      (no-recurse nil)
	      (t (save-excursion (up-list 1) (hargs:sexpression-p t))))))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hargs:actype-get (actype &optional editing-flag)
  "Interactively get and return list of arguments for ACTYPE's parameters.
Current button is being edited when EDITING-FLAG is non-nil."
  (when editing-flag
    (setq editing-flag t))
  (hargs:action-get (actype:action-body actype) editing-flag))

(defun hargs:at-p (&optional no-default)
  "Return thing at point, if of hargs:reading-type type, or default.
If optional argument NO-DEFAULT is non-nil, nil is returned instead of any
default values.

Caller should have checked whether an argument is presently being read
and has set `hargs:reading-type' to an appropriate argument type.
Handles all of the interactive argument types that `hargs:iform-read' does."
  (cond ;; vertico-mode
	((and (null hargs:reading-type)
	      (bound-and-true-p vertico-mode)
	      ;; Ensure vertico is prompting for an argument
	      (vertico--command-p nil (current-buffer))
	      (active-minibuffer-window))
	 (cond
	  ((or
	    ;; Action Key press
	    (and action-key-depressed-flag
		 (eq (selected-window) (active-minibuffer-window)))
	    ;; Action Mouse Key press
	    (and action-key-release-args
		 (fboundp #'vertico-mouse--index)
		 (eq (posn-window (event-end action-key-release-args))
		     (active-minibuffer-window))))
	   (with-selected-window (active-minibuffer-window)
	     (let ((index (when (and action-key-release-args
				     (fboundp #'vertico-mouse--index))
			    (vertico-mouse--index action-key-release-args)))
		   mini
		   mini-to-point)
	       (if index
		   (save-excursion
		     (vertico--goto index)
		     (vertico--update t)
		     (vertico--candidate))
		 ;; Assume event occurred within the minibufer-contents
		 ;; and return just the contents before point so
		 ;; that those after are deleted and more
		 ;; completions are shown.
		 (setq mini (minibuffer-contents-no-properties))
		 ;; The minibuffer may have some read-only contents
		 ;; at the beginning, e.g. M-x, not included in the 'mini'
		 ;; string, so we have to offset the max index into
		 ;; the string in such cases and protect against
		 ;; when point is set into this read-only area with
		 ;; the 'max' call below.
		 (setq mini-to-point (substring mini 0 (max (- (point) (point-max)) (- (length mini)))))
		 (list (if (and (= (point) (point-max)) (string-empty-p mini-to-point))
			   mini
			 mini-to-point)
		       nil)))))
	  (t (list (vertico--candidate) t))))
	((and (null hargs:reading-type)
	      (or
	       ;; Action Key press
	       (and action-key-depressed-flag
		    (eq (selected-window) (active-minibuffer-window)))
	       ;; Action Mouse Key press
	       (and action-key-release-args
		    (eq (posn-window (event-end action-key-release-args))
			(active-minibuffer-window)))))
	 ;; Event occurred within the minibufer-contents.  Return
	 ;; just the contents before point so that those after are
	 ;; deleted and more completions are shown.
	 (let* ((mini (minibuffer-contents-no-properties))
		(mini-to-point (substring mini 0 (max (- (point) (point-max)) (- (length mini))))))
	   (list (if (and (= (point) (point-max)) (string-empty-p mini-to-point))
		     mini
		   mini-to-point)
		 nil)))
	((and (eq hargs:reading-type 'kcell)
	      (derived-mode-p 'kotl-mode)
	      (not (looking-at "^$")))
	 (kcell-view:label))
	((and (eq hargs:reading-type 'klink)
	      (not (looking-at "^$")))
	 (if (derived-mode-p 'kotl-mode)
	     ;; Here we get the button src default-directory, not the
	     ;; default-directory of the linked to kcell.
	     (list (kcell-view:reference nil (hattr:get 'hbut:current 'dir)))
	   (let ((hargs:reading-type 'file))
	     (list (hargs:at-p)))))
	((eq hargs:reading-type 'kvspec)
	 (read-string "Koutline view spec: "
		      (when (boundp 'kvspec:current) kvspec:current)))
	((eolp) nil)
	((and (eq hargs:reading-type 'hmenu)
	      (eq (selected-window) (minibuffer-window)))
	   (char-to-string
	    (save-excursion
	      (cond ((save-excursion (search-forward ">" nil t))
		     ;; In menu prefix
		     1)
		    ((search-backward " " nil t)
		     ;; On a menu item, return the first capitalized
		     ;; char or if none, the first character
		     (skip-chars-forward " ")
		     (let (case-fold-search)
		       (if (looking-at "[^ \t\nA-Z]*[A-Z]")
			   (char-before (match-end 0))
			 (following-char))))
		    ;; At the end of the menu
		    (t 0)))))
	((let ((completion (hargs:completion t)))
	   (when completion
	     (list completion t))))
	((eq hargs:reading-type 'ebut) (ebut:label-p 'as-label))
	((eq hargs:reading-type 'ibut) (ibut:label-p 'as-label))
	((eq hargs:reading-type 'gbut)
	 (when (eq (current-buffer) (get-file-buffer (gbut:file)))
	   (hbut:label-p 'as-label)))
	((eq hargs:reading-type 'hbut) (hbut:label-p 'as-label))
	((hbut:label-p) nil)
	((eq hargs:reading-type 'file)
	 (cond ((derived-mode-p 'dired-mode)
		(let ((file (dired-get-filename nil t)))
		  (and file (hpath:absolute-to file))))
	       ;; Delimited file name.
	       ((hpath:at-p 'file))
	       ;; Unquoted remote file name.
	       ((hpath:is-p (hpath:remote-at-p) 'file))
	       (no-default nil)
	       ;; Possibly non-existent file name
	       ((hpath:at-p 'file 'non-exist))
	       ((hypb:buffer-file-name))))
	((eq hargs:reading-type 'directory)
	 (cond ((derived-mode-p 'dired-mode)
		(let ((dir (or (smart-dired-pathname-up-to-point t)
			       (dired-get-filename nil t))))
		  (and dir (setq dir (hpath:absolute-to dir))
		       (file-directory-p dir) dir)))
	       ;; Delimited directory name.
	       ((hpath:at-p 'directory))
	       ;; Unquoted remote directory name.
	       ((hpath:is-p (hpath:remote-at-p) 'directory))
	       (no-default nil)
	       ;; Possibly non-existent directory name
	       ((hpath:at-p 'directory 'non-exist))
	       (default-directory)))
	((eq hargs:reading-type 'string)
	 (or (hargs:delimited "\"" "\"") (hargs:delimited "'" "'")
	     (hargs:delimited "`" "'")))
	((memq hargs:reading-type '(actype actypes))
	 (let ((name (hargs:find-tag-default)))
	   (car (set:member name (htype:names 'actypes)))))
	((memq hargs:reading-type '(ibtype ibtypes))
	 (let ((name (hargs:find-tag-default)))
	   (car (set:member name (htype:names 'ibtypes)))))
	((eq hargs:reading-type 'sexpression) (hargs:sexpression-p))
	((memq hargs:reading-type '(Info-index-item Info-node))
	 (when (eq major-mode 'Info-mode)
	   (let ((file (Info-current-filename-sans-extension))
		 (node (cond ((Info-note-at-p))
			     ((Info-menu-item-at-p))
			     (t Info-current-node))))
	     (cond ((and (stringp node) (string-match "\\`\(" node))
		    node)
		   (file
		    (concat "(" file ")" node))
		   (t node)))))
	((eq hargs:reading-type 'mail)
	 (and (hmail:reader-p) (hypb:buffer-file-name)
	      (prin1-to-string (list (rmail:msg-id-get) (hypb:buffer-file-name)))))
	((eq hargs:reading-type 'symbol)
	 (let ((sym (hargs:find-tag-default)))
	   (when (or (fboundp sym) (boundp sym)) sym)))
	((eq hargs:reading-type 'buffer)
	 (let ((tag (hargs:find-tag-default)))
	   (if (member tag (mapcar #'buffer-name (buffer-list)))
	       tag
	     (buffer-name))))
	((eq hargs:reading-type 'character)
	 (following-char))
	((eq hargs:reading-type 'key)
	 (require 'hib-kbd)
	 (let ((key-seq (hbut:label-p 'as-label "{" "}")))
	   (when key-seq (kbd-key:normalize key-seq))))
	((eq hargs:reading-type 'integer)
	 (save-excursion (skip-chars-backward "-0-9")
			 (when (looking-at "-?[0-9]+")
			   (read (current-buffer)))))))

(defun hargs:completion (&optional no-insert)
  "If in the completions buffer, return completion at point.
Also insert unless optional NO-INSERT is non-nil.
Insert in minibuffer if active or in other window if minibuffer is inactive."
  (interactive '(nil))
  (when (or (string-match "[* ]Completions\\*\\'" (buffer-name))
	    (eq major-mode 'completion-mode)
	    (and (boundp 'which-key--buffer)
		 (eq (window-buffer action-key-depress-window) which-key--buffer)
		 (eq (window-buffer action-key-release-window) which-key--buffer)))
    (let ((opoint (point))
	  (owind (selected-window)))
      (when (re-search-backward "^\\|\t\\| [ \t]" nil t)
	(let ((insert-window
	       (cond ((> (minibuffer-depth) 0)
		      (minibuffer-window))
		     ((not (eq (selected-window) (next-window nil)))
		      (next-window nil))))
	      (bury-completions)
	      (entry))
	  (skip-chars-forward " \t")
	  (when (and insert-window
		     ;; Allow single spaces in the middle of completions
		     ;; since completions always end with either a tab,
		     ;; newline or two whitespace characters.
		     (looking-at
		      "[^ \t\n]+\\( [^ \t\n]+\\)*\\( [ \t\n]\\|[\t\n]\\|\\'\\)"))
	    (setq entry (hypb:get-completion))
	    (select-window insert-window)
	    (let ((str (or hargs:string-to-complete
			   (buffer-substring
			    (point)
			    (save-excursion (beginning-of-line)
					    (point))))))
	      (cond
	       ((and (eq (selected-window) (minibuffer-window)))
		(cond ((string-match (concat
				      (regexp-quote entry)
				      "\\'")
				     str)
		       ;; If entry matches tail of minibuffer
		       ;; prefix already, then return minibuffer
		       ;; contents as the entry.
		       (setq entry str))
		      ;;
		      ((string-match "[~/][^/]*\\'" str)
		       ;; file or directory entry
		       (setq entry
			     (concat
			      (substring
			       str 0
			       (1+ (match-beginning 0)))
			      entry))))
		(or no-insert
		    (when entry
		      (if (eq insert-window (minibuffer-window))
			  (delete-minibuffer-contents)
			(erase-buffer))
		      (insert entry))))
	       ;; In buffer, non-minibuffer completion.
	       ;; Only insert entry if last buffer line does
	       ;; not end in entry.
	       (no-insert)
	       ((or (string-match
		     (concat (regexp-quote entry) "\\'") str)
		    (null entry))
		(setq bury-completions t))
	       (t (insert entry)))))
	  (select-window owind) (goto-char opoint)
	  (when bury-completions
	    (bury-buffer nil)
	    (delete-window))
	  entry)))))

(defun hargs:iform-read (iform &optional default-args)
  "Read action arguments according to IFORM, a list with car = `interactive'.
With optional DEFAULT-ARGS equal to t, the current button is being edited, so
its attribute values should be presented as defaults.  Otherwise, use
DEFAULT-ARGS as a list of defaults to present when reading arguments.
See also documentation for `interactive'."
  ;; This is mostly a translation of `call-interactively' to Lisp.
  ;;
  ;; Save the prefix arg now, since use of minibuffer will clobber it
  (setq prefix-arg current-prefix-arg)
  (when (and (listp iform) (eq (car iform) 'interactive))
    (setq iform (cadr iform)))
  (unless (or (null iform) (and (stringp iform) (equal iform "")))
    (unless (or (stringp iform) (listp iform))
      (error "(hargs:iform-read): `iform' must be either a non-empty interactive string or a list whose car = 'interactive, not:\n%S"
	     iform))
    (if (eq default-args t)
	(setq default-args (hattr:get 'hbut:current 'args)
	      ;; Set hargs:defaults global used by "hactypes.el"
	      hargs:defaults default-args)
      (setq hargs:defaults nil))
    (setq hargs:reading-type t)
    (if (not (stringp iform))
	(eval iform)
      (let ((i 0) (start 0) (end (length iform))
	    (ientry) (results) (val) (default))
	;;
	;; Handle special initial interactive string chars.
	;;
	;;   `*' means error if buffer is read-only.
	;;   Notion of when action cannot be performed due to
	;;   read-only buffer is view-specific, so here, we just
	;;   ignore a read-only specification since it is checked for
	;;   earlier by any ebut edit code.
	;;
	;;   `@' means select window of last mouse event.
	;;
	;;   `^' means activate/deactivate mark depending on invocation thru shift translation
	;;   See `this-command-keys-shift-translated' for an explanation.
	;;
	;;   `_' means keep region in same state (active or inactive)
	;;   after this command.
	;;
	(while (cond
		((eq (aref iform i) ?*))
		((eq (aref iform i) ?@)
		 (hargs:select-event-window)
		 t)
		((eq (aref iform i) ?^)
		 (handle-shift-selection))
		((eq (aref iform i) ?_)
		 (push 'only transient-mark-mode)))
	  (setq i (1+ i) start i))
	;;
	(while (and (< start end)
		    (string-match "\n\\|\\'" iform start))
	  (setq start (match-end 0)
		ientry (substring iform i (match-beginning 0))
		i start
		default (car default-args)
		default (if (or (null default) (stringp default))
			    default
			  (prin1-to-string default))
		val (hargs:get ientry default (car results))
		default-args (cdr default-args)
		results (cond ((or (null val) (not (listp val)))
			       (cons val results))
			      ;; Is a list of args?
			      ((eq (car val) 'args)
			       (append (nreverse (cdr val)) results))
			      (t ;; regular list value
			       (cons val results)))))
	(nreverse results)))))


(defun hargs:read (prompt &optional predicate default err val-type)
  "PROMPT without completion for a value matching PREDICATE and return it.
PREDICATE is an optional boolean function of one argument.  Optional DEFAULT
is a string to insert after PROMPT as the default return value.  Optional
ERR is a string to display temporarily when an invalid value is given.
Optional VAL-TYPE is a symbol indicating the type of value to be read.  If
VAL-TYPE equals `sexpression', then return that type; otherwise return the
string read or nil."
  (let ((bad-val) (val) (stringify)
	(prev-reading-p hargs:reading-type)
	(read-func)
	(owind (selected-window))
	(obuf (current-buffer)))
    (unwind-protect
	(progn
	  (cond ((or (null val-type) (eq val-type 'sexpression))
		 (setq read-func 'read-minibuffer
		       hargs:reading-type 'sexpression))
		(t (setq read-func 'read-string hargs:reading-type val-type
			 stringify t)))
	  (while (progn (and default (not (stringp default))
			     (setq default (prin1-to-string default)))
			(condition-case ()
			    (or bad-val (setq val (funcall read-func prompt default)))
			  (error (setq bad-val t)))
			(if bad-val
			    t
			  (and stringify
			       ;; Remove any double quoting of strings.
			       (string-match "\\`\"\\([^\"]*\\)\"\\'" val)
			       (setq val (match-string 1 val)))
			  (and predicate (not (funcall predicate val)))))
	    (if bad-val (setq bad-val nil) (setq default val))
	    (beep)
	    (when err
	      (message err)
	      (sit-for 3)))
	  val)
      (setq hargs:reading-type prev-reading-p)
      (select-window owind)
      (switch-to-buffer obuf))))

(defun hargs:read-buffer-name (prompt)
  "Use PROMPT to read an existing buffer name and return it."
  (hargs:read-match prompt (mapcar #'buffer-name (buffer-list)) nil t nil 'buffer))


(defun hargs:read-match (prompt collection
			 &optional predicate must-match initial-input val-type)
  "PROMPT with completion for a value in COLLECTION and return it.
COLLECTION may be a list of strings, an alist, an obarray (for
`symbol-name' completion) or a hash collection.  COLLECTION may
also be a function to do the completion itself.  Optional
PREDICATE limits completion to a subset of COLLECTION.  Optional
MUST-MATCH means value returned must be from COLLECTION and not
blank (unless INITIAL-INPUT is given as an empty string).
Optional INITIAL-INPUT is a string inserted after PROMPT as the
default value.  Optional VAL-TYPE is a symbol indicating the type
of value to be read."
  (unless (and must-match (null collection))
    (let ((prev-reading-p hargs:reading-type)
	  (completion-ignore-case t)
	  (owind (selected-window))
	  (obuf (current-buffer))
	  result)
      (unwind-protect
	  (progn
	    (setq hargs:reading-type (or val-type t))
	    (while (null result)
	      (setq result (completing-read prompt collection predicate must-match initial-input))
	      (when (and must-match (stringp result) (string-blank-p result)
			 (not (equal initial-input "")))
		(setq result initial-input)
		(when (not initial-input)
		  (beep))))
	    result)
	(setq hargs:reading-type prev-reading-p)
	(when (window-live-p owind)
	  (select-window owind)
	  (switch-to-buffer obuf))))))

(defun hargs:select-p (&optional value assist-bool)
  "Return optional VALUE or value in the minibuffer if any, else nil.
If VALUE is a list, it must be of the form:
\(<str-to-compare-against-minibuffer-contents> <is-exact-completion-flag>).
The first argument is then set to VALUE.  If
<is-exact-completion-flag> is non-null, then Hyperbole will not try to
show completions for VALUE when standard completion is used.

If VALUE is the same as the contents of the minibuffer, it is
used as the desired minibuffer argument and the minibuffer is
exited; otherwise, the minibuffer is erased and VALUE is inserted
there.  Optional ASSIST-BOOL non-nil triggers display of
Hyperbole menu item help when appropriate."
  (when (and (> (minibuffer-depth) 0) (or value (setq value (hargs:at-p))))
    (let ((owind (selected-window)) (back-to)
	  ;; This command requires recursive minibuffers.
	  (enable-recursive-minibuffers t)
	  mini)
      (when (stringp value)
	(setq value (list value nil)))
      (unwind-protect
	  (cl-destructuring-bind (str-value exact-completion-flag) value
	    (setq str-value (and str-value (format "%s" str-value)))
	    (select-window (minibuffer-window))
	    (set-buffer (window-buffer (minibuffer-window)))
	    (setq mini (minibuffer-contents-no-properties))
	    (cond
	     ;;
	     ;; Selecting a Hyperbole minibuffer menu item
	     ((eq hargs:reading-type 'hmenu)
	      (when assist-bool
		(setq hargs:reading-type 'hmenu-help))
	      (hui:menu-enter str-value))
	     ;;
	     ;; Exit minibuffer and use its existing value as the desired parameter
	     ;; if value matches a completion and the minibuffer contents.
	     ;;
	     ;; with vertico-mode
	     ((and (bound-and-true-p vertico-mode)
		   ;; Ensure vertico is prompting for an argument
		   (vertico--command-p nil (current-buffer))
		   (string-equal str-value mini)
		   (vertico--match-p str-value))
	      (goto-char (point-max))
	      (vertico-exit))
	     ;; with ivy-mode
	     ((and (bound-and-true-p ivy-mode)
		   (string-equal str-value mini)
		   (hargs:match-p str-value))
	      (goto-char (point-max))
	      (if assist-bool
		  (ivy-dispatching-done)
		(ivy-done)))
	     ;; with standard minibuffer completion
	     ((and (string-equal str-value mini)
		   (hargs:match-p str-value))
	      (goto-char (point-max))
	      (exit-minibuffer))
	     ;;
	     ;; Value is different than minibuffer contents; clear
	     ;; minibuffer and insert value.
	     (t
	      (delete-minibuffer-contents)
	      (goto-char (point-max))
	      (cond
	       ;; with vertico-mode
	       ((and (bound-and-true-p vertico-mode)
		     ;; Ensure vertico is prompting for an argument
		     (vertico--command-p nil (current-buffer)))
                (if str-value
		    (insert str-value)
		  (vertico-insert))
		(vertico--update t))
	       ;; with ivy-mode
	       ((bound-and-true-p ivy-mode)
		(insert str-value)
		(if assist-bool
		    (ivy-dispatching-done)
		  (ivy-done)))
	       ;; with standard minibuffer completion
	       (t
		(insert str-value)
		(unless exact-completion-flag
		  (minibuffer-completion-help))
		(setq back-to t)))
	      value)))
	(when (and back-to (window-live-p owind))
	  (select-window owind))))))

(provide 'hargs)

;;; hargs.el ends here
