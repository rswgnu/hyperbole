;;; hib-kbd.el --- Implicit button type for key sequences delimited with {}.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    22-Nov-91 at 01:37:57
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;   A press of the Action Key on any sequence of keys delimited by braces
;;   executes its command binding or Hyperbole minibuffer menu binding.
;;
;;   A press of the Assist Key on any sequence of keys delimited by braces
;;   displays the documentation for it.
;;
;;   Sequences of keys should be in human readable string form with spaces
;;   between each key, may contain any number of individual key sequences
;;   and the whole thing should be delimited by braces, e.g. {M-x apropos
;;   RET hyperbole RET}.  Forms such as {\C-b}, {\^b}, and {^b} will not be
;;   recognized.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hactypes)

(defvar kbd-key:named-key-list
  '("add" "backspace" "begin" "bs" "clear" "decimal" "delete" "del"
    "divide" "down" "end" "enter" "esc" "home" "left" "insert"
    "multiply" "newline" "next" "prior" "return" "ret" "right" "rtn"
    "subtract" "tab" "up")
  "List of dedicated keyboard key names which may be used with modifier keys.  Function keys are handled elsewhere.")

(defvar kbd-key:named-key-regexp
  (concat
   (mapconcat 'downcase kbd-key:named-key-list "\\|")
   "\\|"
   (mapconcat 'upcase kbd-key:named-key-list "\\|"))
  "Regexp that matches to any of the dedicated keyboard key names in lower or uppercase.")

(defvar kbd-key:modified-key-regexp
  (concat "\\(\\([ACHMS]-\\|kp-\\)+\\)\\s-*\\(<?\\<" kbd-key:named-key-regexp "\\>>?"
	  "\\|<?[fF][0-9][0-9]?>?\\|<[a-zA-Z0-9]+>\\|.\\)")
  "Regexp matching to a single modified keyboard key within a human-readable string.
Group 1 matches to the set of modifier keys.  Group 3 matches to the unmodified key.")

;;; ************************************************************************
;;; Public implicit button types
;;; ************************************************************************

(defact kbd-key (key-series)
  "Execute a normalized key sequence without curly braces, {}.
KEY-SERIES must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments.

Return t if the sequence appears to be valid, else nil."
  (interactive "kKey sequence to execute (no {}): ")
  (kbd-key:act key-series))

(defib kbd-key ()
  "Execute a key sequence found around point, delimited by curly braces, {}, if any.
Key sequences should be in human readable form, e.g. {C-x C-b}, or what `key-description' returns.
Forms such as {\C-b}, {\^b}, and {^M} will not be recognized.

Any key sequence must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments."
  (unless (or (br-in-browser)
	      (and (looking-at "[{}]") (/= ?\\ (preceding-char))))
    ;; handle long series, e.g. eval-elisp actions
    (let* ((ebut:max-len (max 3000 ebut:max-len))
	   (seq-and-pos (or (hbut:label-p t "{`" "'}" t)
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
      (when (memq (char-before start) '(nil ?\ ?\t ?\n ?\j ?\f ?\"))
	(when (and (stringp key-series)
		   (not (eq key-series "")))
	  (setq key-series (kbd-key:normalize key-series)
		binding (key-binding (kbd key-series))))
	(and (stringp key-series)
	     (or (and binding (not (integerp binding)))
		 (kbd-key:special-sequence-p key-series))
	     (ibut:label-set seq-and-pos)
	     (hact 'kbd-key key-series))))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun kbd-key:act (key-series)
  "Execute the command binding for normalized KEY-SERIES.
Returns t if KEY-SERIES has a binding, else nil."
  (interactive "kKeyboard key to execute (no {}): ")
  (setq current-prefix-arg nil) ;; Execution of the key-series may set it.
  (let ((binding (key-binding (kbd key-series))))
    (cond ((null binding)
	   ;; If this is a special key seqence, execute it by adding
	   ;; its keys to the stream of unread command events.
	   (when (kbd-key:special-sequence-p key-series)
             (kbd-key:key-series-to-events key-series)
	     t))
	  ((memq binding '(action-key action-mouse-key hkey-either))
	   (beep)
	   (message "(kbd-key:act): This key does what the Action Key does.")
	   t)
	  (t (call-interactively binding) t))))

(defun kbd-key:key-series-to-events (key-series)
  "Insert the key-series as a series of keyboard events into Emacs' unread input stream."
  (setq unread-command-events (nconc unread-command-events (listify-key-sequence (kbd key-series)))))

(defun kbd-key:doc (key-series &optional full)
  "Show first line of doc for binding of keyboard KEY-SERIES in minibuffer.
With optional prefix arg FULL, display full documentation for command."
  (interactive "kKey sequence: \nP")
  (let* ((keys (kbd-key:normalize key-series))
	 (cmd  (let ((cmd (key-binding (kbd keys))))
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
    (when kbd-key
      (kbd-key:doc kbd-key t))))

(defun kbd-key:normalize (key-series)
  "Normalize a human-readable string of keyboard keys, KEY-SERIES (without any surrounding {}).
Return the normalized but still human-readable format.
Use `kbd-key:key-series-to-events' to add the key series to Emacs'
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
		 (substring)
		 (arg))
	     (setq norm-key-series (kbd-key:mark-spaces-to-keep norm-key-series "(" ")")
		   norm-key-series (kbd-key:mark-spaces-to-keep norm-key-series "\\[" "\\]")
		   norm-key-series (kbd-key:mark-spaces-to-keep norm-key-series "<" ">")
		   norm-key-series (kbd-key:mark-spaces-to-keep norm-key-series "\"" "\"")
		   norm-key-series (hypb:replace-match-string
				    "<DEL>\\|<DELETE>\\|@key{DEL}\\|\\<DEL\\>" norm-key-series " DEL " t)
		   norm-key-series (hypb:replace-match-string
				    "<BS>\\|<BACKSPACE>\\|@key{BS}\\|\\<BS\\>" norm-key-series " BS " t)
		   norm-key-series (hypb:replace-match-string
				    "<RET>\\|<RTN>\\|<RETURN>\\|@key{RET}\\|@key{RTN}\\|\\<RETURN\\>\\|\\<RET\\>\\|\\<RTN\\>"
				    norm-key-series " RET " t)
		   norm-key-series (hypb:replace-match-string
				    "<TAB>\\|@key{TAB}\\|\\<TAB\\>" norm-key-series " TAB " t)
		   ;; Includes conversion of spaces-to-keep markup to
		   ;; SPC; otherwise, later calls to `kbd' will remove
		   ;; these spaces.
		   norm-key-series (hypb:replace-match-string
				    "\\\\ \\|\0\0\0\\|<SPC>\\|@key{SPC}\\|\\<SPC\\>" norm-key-series " SPC " t)
		   norm-key-series (hypb:replace-match-string
				    "<ESC>\\|<ESCAPE>\\|@key{ESC}\\|\\<ESC\\(APE\\)?\\>" norm-key-series " M-" t)
		   ;; ESC ESC
		   norm-key-series (hypb:replace-match-string
				    "M-\\s-*M-" norm-key-series " ESC M-" t)
		   ;; Separate with a space any keys with a modifier
		   norm-key-series (hypb:replace-match-string kbd-key:modified-key-regexp
							      norm-key-series " \\1\\3 ")
		   ;; Normalize regular whitespace to single spaces
		   norm-key-series (hypb:replace-match-string "[ \t\n\r\f]+" norm-key-series " " t)

		   ;; Unqote special {} chars.
		   norm-key-series (hypb:replace-match-string "\\\\\\([{}]\\)"
							      norm-key-series "\\1")
		   norm-key-series (hpath:trim norm-key-series))
	     ;; (while (string-match "\\`\\(C-u\\|M-\\)\\(-?[0-9]+\\)" norm-key-series)
	     ;;   (setq arg (string-to-number (match-string 2 norm-key-series))
	     ;; 	     norm-key-series (substring norm-key-series (match-end 0))))

	     (unless (string-empty-p norm-key-series)
	       (hypb:mark-object norm-key-series))
	     norm-key-series)))
	(t (error "(kbd-key:normalize): requires a string argument, not `%s'" key-series))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kbd-key:extended-command-p (key-series)
  "Return non-nil if the string KEY-SERIES is a normalized extended command invocation, i.e. M-x command."
  (when (stringp key-series)
    (string-match kbd-key:extended-command-prefix key-series)))

(defun kbd-key:hyperbole-hycontrol-key-p (key-series)
  "Return t if normalized, non-nil KEY-SERIES is given when in a HyControl mode, else nil.
Allows for multiple key sequences strung together."
  (and key-series
       (featurep 'hycontrol)
       (or hycontrol-windows-mode hycontrol-frames-mode)
       ;; If wanted to limit to single key bindings and provide tighter checking:
       ;;   (string-match "[-.0-9]*\\(.*\\)" key-series)
       ;;   (key-binding (kbd (match-string 1 key-series)))
       t))

(defun kbd-key:hyperbole-mini-menu-key-p (key-series)
  "Return t if normalized KEY-SERIES appears to invoke a Hyperbole menu item or sequence of keys, else nil.
Also, initialize `kbd-key:mini-menu-key' to the key sequence that invokes the Hyperbole minibuffer menu."
  (when key-series
    (unless kbd-key:mini-menu-key
      (setq kbd-key:mini-menu-key (regexp-quote (kbd-key:normalize (key-description (car (where-is-internal 'hyperbole)))))))
    (when (string-match kbd-key:mini-menu-key key-series)
      t)))

(defun kbd-key:key-and-arguments (key-series)
  "Return t if normalized KEY-SERIES appears to be a bound key sequence possibly with following interactive arguments, else nil."
  (let ((prefix-binding (and (stringp key-series) (key-binding (kbd (substring key-series 0 (seq-position key-series ?\ )))))))
    ;; Just ensure that 1st character is bound to something that is
    ;; not a self-insert-command or a number.
    (and prefix-binding
	 (not (or (integerp prefix-binding)
		  (eq prefix-binding 'self-insert-command)))
	 t)))

(defun kbd-key:mark-spaces-to-keep (string start-delim end-delim)
  "Return STRING with all spaces between any START-DELIM string and END-DELIM string marked for non-replacement."
  (let ((regexp (format "\\(%s\\S-*\\)\\s-\\(.*%s\\)"
			start-delim end-delim))
	(start 0)
	(end)
	(substring))
    (while (string-match regexp string start)
      (setq start (match-beginning 0)
	    end (match-end 0)
	    substring (match-string 0 string)
	    string (concat (substring string 0 start)
			   (hypb:replace-match-string "\\s-" substring "\0\0\0" t)
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
  (kbd-key:normalize (key-description (where-is-internal 'execute-extended-command (current-global-map) t)))
  "Normalized prefix string that invokes an extended command; typically ESC x.")

(defvar kbd-key:mini-menu-key nil
  "The key sequence that invokes the Hyperbole minibuffer menu.")
;; Set above variable
(kbd-key:hyperbole-mini-menu-key-p "")


(provide 'hib-kbd)

;;; hib-kbd.el ends here
