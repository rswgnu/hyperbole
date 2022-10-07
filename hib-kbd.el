;;; hib-kbd.el --- Implicit button type for key sequences delimited with {}.  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    22-Nov-91 at 01:37:57
;; Last-Mod:      7-Oct-22 at 00:06:09 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;   A press of the Action Key on any series of key sequences delimited by
;;   curly braces executes all of the associated commands.  Key sequences
;;   can include Hyperbole minibuffer menu sequences.
;;
;;   A press of the Assist Key on any series of key sequences delimited by
;;   curly braces displays the documentation for it.
;;
;;   Sequences of keys should be in human readable string form with spaces
;;   between each key, may contain any number of individual key sequences
;;   and the whole thing should be delimited by braces, e.g. {M-x apropos
;;   RET hyperbole RET}.  Forms such as {\C-b}, {\^b}, and {^b} will not be
;;   recognized.
;;
;;   Programmatically, to execute a key series given as a string, use:
;;   (kbd-key:execute "{key series}").

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hactypes) ;; This invokes (require 'hargs)
(require 'seq)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************
(defvar kbd-key:named-key-list
  '("add" "backspace" "begin" "bs" "clear" "decimal" "delete" "del"
    "divide" "down" "end" "enter" "esc" "home" "left" "insert"
    "multiply" "newline" "next" "prior" "return" "ret" "right" "rtn"
    "subtract" "tab" "up")
  "List of dedicated keyboard key names which may be used with modifier keys.
Function keys are handled elsewhere.")

(defvar kbd-key:named-key-regexp
  (concat
   (mapconcat 'downcase kbd-key:named-key-list "\\|")
   "\\|"
   (mapconcat 'upcase kbd-key:named-key-list "\\|"))
  "Regexp that matches the dedicated keyboard key names in lower or uppercase.")

(defvar kbd-key:modified-key-regexp
  (concat "\\(\\[?\\([ACHMS]-\\|kp-\\)+\\)[ \t\n\r\f]*\\(\\(<?\\<" kbd-key:named-key-regexp "\\>>?"
	  "\\|<?[fF][0-9][0-9]?>?\\|<[a-zA-Z0-9]+>\\|.\\)\\]?\\)")
  "Regexp matching a single modified keyboard key within a human-readable string.
Group 1 matches to the set of modifier keys.  Group 3 matches to
the unmodified key.")

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************
(declare-function helm-mode "ext:helm")

;;; ************************************************************************
;;; Public implicit button types
;;; ************************************************************************

(defact kbd-key (key-series)
  "Execute a normalized KEY-SERIES (series of key sequences) without curly braces.
Each key sequence within KEY-SERIES must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments.

Return t if the sequence appears to be valid, else nil."
  (interactive "sKey series to execute (no {}): ")
  (kbd-key:act key-series))

(defib kbd-key ()
  "Execute a key series (series of key sequences) around point.
The key series is delimited by curly braces, {}.  Key sequences
should be in human readable form, e.g. {C-x C-b}, or what
`key-description' returns.  Forms such as {\C-b}, {\^b}, and {^M}
will not be recognized.

Any key sequence within the series must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments."
  (unless (or (br-in-browser)
	      (and (looking-at "[{}]") (/= ?\\ (preceding-char))))
    ;; Temporarily make open and close braces have list syntax for
    ;; matching purposes.
    (let ((open-brace-syntax (hypb:get-raw-syntax-descriptor ?\{))
	  (close-brace-syntax (hypb:get-raw-syntax-descriptor ?\})))
      (unwind-protect
	  (progn (modify-syntax-entry ?\{ "(}" (syntax-table))
		 (modify-syntax-entry ?\} "){" (syntax-table))
		 ;; Handle long series, e.g. eval-elisp actions
		 (let* ((hbut:max-len (max 3000 (hbut:max-len)))
			(seq-and-pos (or
				      ;; (kbd) calls but only if point is between double quotes
 				      (and (hbut:label-p t "(kbd \"" "\"\)" t)
					   (hbut:label-p t "\"" "\"\)" t))
				      ;; braces delimiters
				      (hbut:label-p t "{`" "'}" t)
				      (hbut:label-p t "{" "}" t)
				      ;; Regular dual single quotes (Texinfo smart quotes)
				      (hbut:label-p t "``" "''" t)
				      ;; Typical GNU manual key sequences; note
				      ;; these are special quote marks, not the
				      ;; standard ASCII characters.
				      (hbut:label-p t "‘" "’" t)))
			;; This excludes delimiters
			(key-series (car seq-and-pos))
			(start (cadr seq-and-pos))
			binding)
		   ;; Match only when start delimiter is preceded by whitespace,
		   ;; double quotes or is the 1st buffer character, so do not
		   ;; match to things like ${variable}.
		   (when (memq (char-before start) '(nil ?\ ?\t ?\n ?\r ?\f ?\"))
		     (when (and (stringp key-series)
				(not (eq key-series "")))
		       ;; Replace any ${} internal or env vars; leave
		       ;; $VAR untouched for the shell to evaluate.
		       (let ((hpath:variable-regexp "\\${\\([^}]+\\)}"))
			 (setq key-series (hpath:substitute-value key-series)))

		       (setq key-series (kbd-key:normalize key-series)
			     binding (kbd-key:binding key-series)))
		     (and (stringp key-series)
			  (or (and binding (not (integerp binding)))
			      (kbd-key:special-sequence-p key-series))
			  (ibut:label-set seq-and-pos)
			  (hact 'kbd-key key-series)))))
	(hypb:set-raw-syntax-descriptor ?\{ open-brace-syntax)
	(hypb:set-raw-syntax-descriptor ?\} close-brace-syntax)))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun kbd-key:act (key-series)
  "Execute the normalized KEY-SERIES.
Return t if KEY-SERIES appears valid, else nil."
  (interactive "sKey series to execute (no {}): ")
  (setq current-prefix-arg nil) ;; Execution of the key-series may set it.
  (let ((binding (kbd-key:binding key-series)))
    (cond ((null binding)
	   (if (kbd-key:special-sequence-p key-series)
	       (kbd-key:execute-special-series key-series)
	     (kbd-key:key-series-to-events key-series))
	   t)
	  ((memq binding '(action-key action-mouse-key hkey-either))
	   (beep)
	   (message "(kbd-key:act): This key does what the Action Key does.")
	   t)
	  ((not (integerp binding))
	   (call-interactively binding)
	   t))))

(defun kbd-key:execute (key-series)
  "Execute a possibly non-normalized KEY-SERIES.
The KEY-SERIES can be with or without curly brace delimiters.
Return t if KEY-SERIES is a valid key series that is executed, else nil."
  (interactive "sKey series to execute: ")
  (when (and key-series
	     (setq key-series (kbd-key:is-p key-series)))
    (hact #'kbd-key:act key-series)))

(defun kbd-key:execute-special-series (key-series)
  "Execute key series."
  (if (memq (key-binding [?\M-x]) #'(execute-extended-command counsel-M-x))
      (kbd-key:key-series-to-events key-series)
    ;; Disable helm while processing M-x commands; helm
    ;; gobbles final RET key.  Counsel works without modification.
    (let ((orig-binding (global-key-binding [?\M-x]))
	  (helm-flag (when (boundp 'helm-mode) helm-mode))
	  (minibuffer-completion-confirm))
      (unwind-protect
	  (progn
	    (when helm-flag (helm-mode -1))
	    (global-set-key [?\M-x] 'execute-extended-command)
	    (kbd-key:key-series-to-events key-series))
	(kbd-key:key-series-to-events
	 (format "M-: SPC (kbd-key:maybe-enable-helm SPC %s SPC #'%S) RET"
		 helm-flag orig-binding))))))

(defun kbd-key:maybe-enable-helm (helm-flag orig-M-x-binding)
  "Enable helm-mode if HELM-FLAG is non-nil.
Restore M-x binding to ORIG-M-X-BINDING."
  (when helm-flag (helm-mode 1))
  (global-set-key [?\M-x] orig-M-x-binding))

(defun kbd-key:key-series-to-events (key-series)
  "Insert the key-series as a series of keyboard events.
The events are inserted into Emacs unread input stream.  Emacs
then executes them when its command-loop regains control."
  (setq unread-command-events (nconc unread-command-events
				     (listify-key-sequence
				      (kbd-key:kbd key-series)))))

(defun kbd-key:doc (key-series &optional full)
  "Show first line of doc for binding of keyboard KEY-SERIES in minibuffer.
With optional prefix arg FULL, display full documentation for command."
  (interactive "kKey sequence: \nP")
  (let* ((keys (kbd-key:normalize key-series))
	 (cmd  (let ((cmd (kbd-key:binding keys)))
		 (unless (integerp cmd) cmd)))
	 (doc (and cmd (documentation cmd)))
	 (end-line))
    (cond (cmd
	   (if doc
	       (or full
		   (setq end-line (string-match "[\n]" doc)
			 doc (substitute-command-keys (substring doc 0 end-line))))
	     (setq doc (format "No documentation for {%s} %s" key-series (or cmd ""))))
	   (if (and cmd doc)
	       (if full
		   (describe-function cmd)
		 (message doc))))
	  ((setq doc (hui:menu-doc keys (not full)))
	   (if full
	       (hui:menu-help doc)
	     (message doc)))
	  (t (hkey-help)))))


(defun kbd-key:help (but)
  "Display documentation for binding of keyboard key given by BUT's label."
  (let ((kbd-key (hbut:key-to-label (hattr:get but 'lbl-key))))
    (when (and kbd-key (not (string-empty-p kbd-key)))
      (kbd-key:doc kbd-key t))))

(defun kbd-key:is-p (str)
  "Return the non-delimited, normalized form, of a delimited key series, STR.
When STR is a curly-brace {} delimited key series, a
non-delimited, normalized form is returned, else nil.  Key
sequences should be in human readable form, e.g. {C-x C-b}, or
what `key-description' returns.  Forms such as {\C-b}, {\^b}, and
{^M} will not be recognized.

Any key sequence within the series must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments."
  ;; Handle long series, e.g. eval-elisp actions
  (let* ((hbut:max-len (max 3000 (hbut:max-len)))
	 ;; STR must include delimiters but they are stripped from `key-series'.
	 (key-series (or (kbd-key:remove-delimiters str "{`" "'}")
			 (kbd-key:remove-delimiters str "{" "}")
			 ;; Regular dual single quotes (Texinfo smart quotes)
			 (kbd-key:remove-delimiters str "``" "''")
			 ;; Typical GNU manual key sequences; note
			 ;; these are special quote marks, not the
			 ;; standard ASCII characters.
			 (kbd-key:remove-delimiters str "‘" "’")))
	 binding)
    (when (and (stringp key-series)
	       (not (eq key-series "")))
      (setq key-series (kbd-key:normalize key-series)
	    binding (kbd-key:binding key-series)))
    (and (stringp key-series)
	 (or (and binding (not (integerp binding)))
	     (kbd-key:special-sequence-p key-series))
	 key-series)))

(defun kbd-key:normalize (key-series)
  "Normalize a human-readable string of keyboard keys, KEY-SERIES.
The KEY-SERIES is without any surrounding {}.  Return the
normalized but still human-readable format.  Use
`kbd-key:key-series-to-events' to add the key series to Emacs'
keyboad input queue, as if they had been typed by the user."
  (interactive "kKeyboard key sequence to normalize (no {}): ")
  ;;
  ;; Hyperbole developers: see  `edmacro-parse-keys' in "edmacro.el"
  ;; for further details on key formats.
  ;;
  (cond	((stringp key-series)
	 (if (hypb:object-p key-series)
	     ;; Prevent multiple normalizations which can strip desired
	     ;; RET and SPC characters.
	     key-series
	   (let ((norm-key-series (copy-sequence key-series))
		 (case-fold-search nil)
		 (case-replace t)
		 ;; (substring)
		 ;; (arg)
		 )
	     (setq norm-key-series (kbd-key:mark-spaces-to-keep norm-key-series "(" ")")
		   norm-key-series (kbd-key:mark-spaces-to-keep norm-key-series "\\[" "\\]")
		   norm-key-series (kbd-key:mark-spaces-to-keep norm-key-series "<" ">")
		   norm-key-series (kbd-key:mark-spaces-to-keep norm-key-series "\"" "\"")
		   norm-key-series (replace-regexp-in-string
				    "<DEL>\\|<DELETE>\\|@key{DEL}\\|\\<DEL\\>" " DEL " norm-key-series nil t)
		   norm-key-series (replace-regexp-in-string
				    "<BS>\\|<BACKSPACE>\\|@key{BS}\\|\\<BS\\>" " BS " norm-key-series nil t)
		   norm-key-series (replace-regexp-in-string
				    "<RET>\\|<RTN>\\|<RETURN>\\|@key{RET}\\|@key{RTN}\\|\\<RETURN\\>\\|\\<RET\\>\\|\\<RTN\\>"
				    " RET " norm-key-series nil t)
		   norm-key-series (replace-regexp-in-string
				    "<TAB>\\|@key{TAB}\\|\\<TAB\\>" " TAB " norm-key-series nil t)
		   ;; Includes conversion of spaces-to-keep markup to
		   ;; SPC; otherwise, later calls to `kbd' will remove
		   ;; these spaces.
		   norm-key-series (replace-regexp-in-string
				    "\\\\ \\|\0\0\0\\|<SPC>\\|@key{SPC}\\|\\<SPC\\>" " SPC " norm-key-series nil t)
		   norm-key-series (replace-regexp-in-string
				    "<ESC>\\|<ESCAPE>\\|@key{ESC}\\|\\<ESC\\(APE\\)?\\>" " M-" norm-key-series nil t)
		   ;; ESC ESC
		   norm-key-series (replace-regexp-in-string
				    "M-[ \t\n\r\f]*M-" " ESC M-" norm-key-series nil t)
		   ;; Separate with a space any keys with a modifier
		   norm-key-series (replace-regexp-in-string kbd-key:modified-key-regexp
							      " \\1\\3 " norm-key-series)
		   ;; Normalize regular whitespace to single spaces
		   norm-key-series (replace-regexp-in-string "[ \t\n\r\f]+" " " norm-key-series nil t)

		   ;; Unqote special {} chars.
		   norm-key-series (replace-regexp-in-string "\\\\\\([{}]\\)"
							      "\\1" norm-key-series)
		   norm-key-series (hpath:trim norm-key-series))
	     ;; (while (string-match "\\`\\(C-u\\|M-\\)\\(-?[0-9]+\\)" norm-key-series)
	     ;;   (setq arg (string-to-number (match-string 2 norm-key-series))
	     ;; 	     norm-key-series (substring norm-key-series (match-end 0))))

	     (unless (string-empty-p norm-key-series)
	       (hypb:mark-object norm-key-series))
	     norm-key-series)))
	(t (error "(kbd-key:normalize): requires a string argument, not `%s'" key-series))))

(defun kbd-key:remove-delimiters (str start-delim end-delim)
  "Return STR sans START-DELIM and END-DELIM (strings).
Return nil if STR does not start and end with the given delimiters."
  (when (and (string-match (format "\\`%s" (regexp-quote start-delim)) str)
	     (string-match (format "%s\\'"  (regexp-quote end-delim)) str))
    (string-trim str start-delim end-delim)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kbd-key:binding (key-series)
  "Return key binding for KEY-SERIES if it is a single key sequence or nil."
  ;; This custom function is used to prevent the (kbd) call from
  ;; mistakenly removing angle brackets from Hyperbole implicit button
  ;; names, like: <[td]>.
  (key-binding (kbd-key:kbd key-series)))

(defun kbd-key:kbd (key-series)
  "Convert normalized KEY-SERIES to a sequence of internal Emacs keys.
For an approximate inverse of this, see `key-description'."
  (kbd-key:parse key-series))

;; Based on 'edmacro-parse-keys' from Emacs "edmacro.el" but does not
;; try to parse <event> strings nor does it have optional second
;; parameter, need-vector.
(defun kbd-key:parse (string)
  (let ((case-fold-search nil)
	(len (length string)) ; We won't alter string in the loop below.
	(pos 0)
	(res []))
    (while (and (< pos len)
		(string-match "[^ \t\n\f]+" string pos))
      (let* ((word-beg (match-beginning 0))
	     (word-end (match-end 0))
	     (word (substring string word-beg len))
	     (times 1)
	     key)
	(setq word (substring string word-beg word-end)
	      pos word-end)
	(when (string-match "\\([0-9]+\\)\\*." word)
	  (setq times (string-to-number (substring word 0 (match-end 1))))
	  (setq word (substring word (1+ (match-end 1)))))
	(cond ((string-match "^<<.+>>$" word)
	       (setq key (vconcat (cond ((memq (key-binding [?\M-x])
					       kbd-key:extended-command-binding-list)
					 [?\M-x])
					((seq-filter
					  (lambda (elt) (car (where-is-internal elt)))
					  kbd-key:extended-command-binding-list)
					 [?\M-x]))
				  (substring word 2 -2) "\r")))
	      ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
		    (progn
		      (setq word (concat (substring word (match-beginning 1)
						    (match-end 1))
					 (substring word (match-beginning 3)
						    (match-end 3))))
		      (not (string-match
			    "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
			    word))))
	       (setq key (list (intern word))))
	      ((or (equal word "REM") (string-match "^;;" word))
	       (setq pos (string-match "$" string pos)))
	      (t
	       (let ((orig-word word) (prefix 0) (bits 0))
		 (while (string-match "^[ACHMsS]-." word)
		   (cl-incf bits (cdr (assq (aref word 0)
					 '((?A . ?\A-\^@) (?C . ?\C-\^@)
					   (?H . ?\H-\^@) (?M . ?\M-\^@)
					   (?s . ?\s-\^@) (?S . ?\S-\^@)))))
		   (cl-incf prefix 2)
		   (cl-callf substring word 2))
		 (when (string-match "^\\^.$" word)
		   (cl-incf bits ?\C-\^@)
		   (cl-incf prefix)
		   (cl-callf substring word 1))
		 (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
					    ("LFD" . "\n") ("TAB" . "\t")
					    ("ESC" . "\e") ("SPC" . " ")
					    ("DEL" . "\177")))))
		   (when found (setq word (cdr found))))
		 (when (string-match "^\\\\[0-7]+$" word)
		   (cl-loop for ch across word
                            for n = 0 then (+ (* n 8) ch -48)
                            finally do (setq word (vector n))))
		 (cond ((= bits 0)
			(setq key word))
		       ((and (= bits ?\M-\^@) (stringp word)
			     (string-match "^-?[0-9]+$" word))
			(setq key (cl-loop for x across word
                                           collect (+ x bits))))
		       ((/= (length word) 1)
			(error "%s must prefix a single character, not %s"
			       (substring orig-word 0 prefix) word))
		       ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
			     ;; We used to accept . and ? here,
			     ;; but . is simply wrong,
			     ;; and C-? is not used (we use DEL instead).
			     (string-match "[@-_a-z]" word))
			(setq key (list (+ bits (- ?\C-\^@)
					   (logand (aref word 0) 31)))))
		       (t
			(setq key (list (+ bits (aref word 0)))))))))
	(when key
	  (cl-loop repeat times do (cl-callf vconcat res key)))))
    (when (and (>= (length res) 4)
	       (eq (aref res 0) ?\C-x)
	       (eq (aref res 1) ?\()
	       (eq (aref res (- (length res) 2)) ?\C-x)
	       (eq (aref res (- (length res) 1)) ?\)))
      (setq res (cl-subseq res 2 -2)))
    (if (cl-loop for ch across res
                 always (and (characterp ch)
                             (let ((ch2 (logand ch (lognot ?\M-\^@))))
                               (and (>= ch2 0) (<= ch2 127)))))
	(concat (cl-loop for ch across res
                         collect (if (= (logand ch ?\M-\^@) 0)
                                     ch (+ ch 128))))
      res)))

(defun kbd-key:extended-command-p (key-series)
  "Return non-nil if the KEY-SERIES is a normalized extended command invocation.
That is, 'M-x command'."
  (when (stringp key-series)
    (string-match kbd-key:extended-command-prefix key-series)))

(defun kbd-key:hyperbole-hycontrol-key-p (key-series)
  "Return t if KEY-SERIES is normalized, non-nil and in HyControl mode, else nil.
Allow for multiple key sequences strung together."
  (and key-series
       (featurep 'hycontrol)
       (or hycontrol-windows-mode hycontrol-frames-mode)
       ;; If wanted to limit to single key bindings and provide tighter checking:
       ;;   (string-match "[-.0-9]*\\(.*\\)" key-series)
       ;;   (kbd-key:binding (match-string 1 key-series))
       t))

(defun kbd-key:hyperbole-mini-menu-key-p (key-series)
  "Return non-nil if KEY-SERIES invoke a Hyperbole menu item or sequence of keys.
KEY-SERIES is normalized.  Also, initialize `kbd-key:mini-menu-key' to the
key sequence that invokes the Hyperbole minibuffer menu."
  (when (stringp key-series)
    (unless (and (stringp kbd-key:mini-menu-key) (not (string-empty-p kbd-key:mini-menu-key)))
      (setq kbd-key:mini-menu-key (regexp-quote (kbd-key:normalize (key-description (car (where-is-internal 'hyperbole)))))))
    (when (string-match kbd-key:mini-menu-key key-series)
      t)))

(defun kbd-key:key-and-arguments (key-series)
  "Return t if normalized KEY-SERIES appears to be a bound key sequence, else nil.
KEY-SERIES can have following interactive arguments."
  (let ((prefix-binding (and (stringp key-series) (kbd-key:binding (substring key-series 0 (seq-position key-series ?\ ))))))
    ;; Just ensure that 1st character is bound to something that is
    ;; not a self-insert-command or a number.
    (and prefix-binding
	 (not (or (integerp prefix-binding)
		  (eq prefix-binding 'self-insert-command)))
	 t)))

(defun kbd-key:mark-spaces-to-keep (string start-delim end-delim)
  "Return STRING with all spaces between START-DELIM and END-DELIM marked to keep."
  (let ((regexp (format "\\(%s[^ \t\n\r\f]*\\)[ \t\n\r\f]\\(.*%s\\)"
			start-delim end-delim))
	(start 0)
	(end)
	(substring))
    (while (string-match regexp string start)
      (setq start (match-beginning 0)
	    end (match-end 0)
	    substring (match-string 0 string)
	    string (concat (substring string 0 start)
			   (replace-regexp-in-string "[ \t\n\r\f]" "\0\0\0" substring nil t)
			   (if (< end (length string))
			       (substring string end)
			     ""))
	    start end))
    string))

(defun kbd-key:special-sequence-p (key-series)
  "Return non-nil if normalized KEY-SERIES string is one of the following:
a Hyperbole minibuffer menu item key sequence,
a HyControl key sequence,
a M-x extended command,
  or a valid key sequence together with its interactive arguments."
  (or (kbd-key:hyperbole-mini-menu-key-p key-series)
      (kbd-key:hyperbole-hycontrol-key-p key-series)
      (kbd-key:extended-command-p key-series)
      (kbd-key:key-and-arguments key-series)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst kbd-key:extended-command-prefix
  (format "\\_<%s\\_>" (kbd-key:normalize "M-x"))
  "Normalized prefix regular expression that invokes an extended command.
Default is M-x.")

(defconst kbd-key:extended-command-binding-list '(execute-extended-command helm-M-x counsel-M-x)
  "List of commands that may be bound to M-x to invoke extended/named commands.")

(defvar kbd-key:mini-menu-key nil
  "The key sequence that invokes the Hyperbole minibuffer menu.")
;; Set above variable
(kbd-key:hyperbole-mini-menu-key-p "")


(provide 'hib-kbd)

;;; hib-kbd.el ends here
