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

;;; ************************************************************************
;;; Public implicit button types
;;; ************************************************************************

(defact kbd-key (key-series)
  "Executes a normalized key sequence without curly braces, {}.
KEY-SERIES must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments.

Returns t if the sequence appears to be valid, else nil."
  (interactive "kKey sequence to execute (no {}): ")
  (kbd-key:act key-series))

(defib kbd-key ()
  "Executes a key sequence found around point, delimited by curly braces, {}, if any.
Key sequences should be in human readable form, e.g. {C-x C-b}, or what `key-description' returns.
Forms such as {\C-b}, {\^b}, and {^b} will not be recognized.

Any key sequence must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments."
  (unless (or (br-in-browser)
	      (and (looking-at "[{}]") (/= ?\\ (preceding-char))))
    (let* ((seq-and-pos (or (hbut:label-p t "{`" "'}" t)
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
      ;; Match only when start delimiter is preceded by whitespace or
      ;; is the 1st buffer character, so do not match to things like ${variable}.
      (when (memq (char-before start) '(nil ?\ ?\t ?\n ?\j ?\f))
	(when (and (stringp key-series)
		   (not (eq key-series "")))
	  (setq key-series (kbd-key:normalize key-series)
		binding (key-binding key-series)))
	(and (stringp key-series)
	     (or (and binding (not (integerp binding)))
		 (kbd-key:special-sequence-p key-series))
	     (ibut:label-set seq-and-pos)
	     (hact 'kbd-key key-series))))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun kbd-key:act (key-series)
  "Executes the command binding for normalized KEY-SERIES.
Returns t if KEY-SERIES has a binding, else nil."
  (interactive "kKeyboard key to execute (no {}): ")
  (setq current-prefix-arg nil) ;; Execution of the key-series may set it.
  (let ((binding (key-binding key-series)))
    (cond ((null binding)
	   ;; If this is a special key seqence, execute it by adding
	   ;; its keys to the stream of unread command events.
	   (when (kbd-key:special-sequence-p key-series)
	     (setq unread-command-events (nconc unread-command-events (mapcar 'identity key-series)))
	     t))
	  ((memq binding '(action-key action-mouse-key hkey-either))
	   (beep)
	   (message "(kbd-key:act): This key does what the Action Key does.")
	   t)
	  (t (call-interactively binding) t))))

(defun kbd-key:doc (key-series &optional full)
  "Shows first line of doc for binding of keyboard KEY-SERIES in minibuffer.
With optional prefix arg FULL, displays full documentation for command."
  (interactive "kKey sequence: \nP")
  (let* ((keys (kbd-key:normalize key-series))
	 (cmd  (let ((cmd (key-binding keys)))
		 (if (not (integerp cmd)) cmd)))
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
    (if kbd-key (kbd-key:doc kbd-key t))))

(defun kbd-key:normalize (key-series)
  "Returns KEY-SERIES string (without surrounding {}) normalized into a form that can be parsed by commands."
  (interactive "kKeyboard key sequence to normalize (no {}): ")
  (if (stringp key-series)
      (let ((norm-key-seq (copy-sequence key-series))
	    (case-fold-search nil)
	    (case-replace t)
	    (substring)
	    (arg))
	(setq norm-key-seq (hypb:replace-match-string
			    "@key{DEL}\\|<DEL>\\|\\<DEL\\>" norm-key-seq "\177" t)
	      norm-key-seq (hypb:replace-match-string
			    "@key{RET}\\|<RET>\\|@key{RTN}\\|\\<RETURN\\>\\|\\<RET\\>\\|\\<RTN\\>"
			    norm-key-seq "$#@!" t)
	      norm-key-seq (hypb:replace-match-string
			    "\\<ESC\s-*ESC\\>" norm-key-seq "\233" t)
	      norm-key-seq (hypb:replace-match-string
			    "@key{ESC}\\|<ESC>\\|\\<ESC\\(APE\\)?\\>" norm-key-seq "M-" t)
	      norm-key-seq (hypb:replace-match-string
			    "C-M-" norm-key-seq "M-C-" t)
	      norm-key-seq (kbd-key:mark-spaces-to-keep norm-key-seq "(" ")")
	      norm-key-seq (kbd-key:mark-spaces-to-keep norm-key-seq "\\[" "\\]")
	      norm-key-seq (kbd-key:mark-spaces-to-keep norm-key-seq "<" ">")
	      norm-key-seq (kbd-key:mark-spaces-to-keep norm-key-seq "\"" "\"")
	      norm-key-seq (hypb:replace-match-string "\\\\ " norm-key-seq "\0\0\0" t)
	      norm-key-seq (hypb:replace-match-string
			    "[ \t\n\r]+" norm-key-seq "" t)
	      norm-key-seq (hypb:replace-match-string
			    "\0\0\0\\|@key{SPC}\\|<SPC>\\|\\<SPC\\>" norm-key-seq "\040" t)
	      norm-key-seq (hypb:replace-match-string "$#@!" norm-key-seq "\015" t)
	      ;; Unqote special {} chars.
	      norm-key-seq (hypb:replace-match-string "\\\\\\([{}]\\)"
						      norm-key-seq "\\1"))
	(while (string-match "\\`\\(C-u\\|M-\\)\\(-?[0-9]+\\)" norm-key-seq)
	  (setq arg
		(string-to-number (substring norm-key-seq (match-beginning 2)
					     (match-end 2)))
		norm-key-seq (substring norm-key-seq (match-end 0))))

	;; Quote Control and Meta key names
	(setq norm-key-seq (hypb:replace-match-string
			    "C-\\(.\\)" norm-key-seq
			    (lambda (str)
			      (char-to-string
			       (1+ (- (downcase
				       (string-to-char
					(substring str (match-beginning 1)
						   (1+ (match-beginning 1)))))
				      ?a)))))
	      norm-key-seq (hypb:replace-match-string
			    "M-\\(.\\)" norm-key-seq
			    (lambda (str)
			      (concat "" (substring str (match-beginning 1)
						      (1+ (match-beginning 1))))))))
    (error "(kbd-key:normalize): requires a string argument, not `%s'" key-series)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kbd-key:extended-command-p (key-series)
  "Returns non-nil if the string KEY-SERIES is a normalized extended command invocation, i.e. M-x command."
  (and (stringp key-series) (string-match kbd-key:extended-command-prefix key-series)))
  
(defun kbd-key:hyperbole-hycontrol-key-p (key-series)
  "Returns t if normalized, non-nil KEY-SERIES is given when in a HyControl mode, else nil.
Allows for multiple key sequences strung together."
  (and key-series
       (featurep 'hycontrol)
       (or hycontrol-windows-mode hycontrol-frames-mode)
       ;; If wanted to limit to single key bindings and provide tighter checking:
       ;;   (string-match "[-.0-9]*\\(.*\\)" key-series)
       ;;   (key-binding (match-string 1 key-series))
       t))

(defun kbd-key:hyperbole-mini-menu-key-p (key-series)
  "Returns t if normalized KEY-SERIES appears to invoke a Hyperbole menu item or sequence of keys, else nil."
  (when key-series
    (let ((mini-menu-key (kbd-key:normalize (key-description (car (where-is-internal 'hyperbole))))))
      (if (string-match (regexp-quote mini-menu-key) key-series) t))))

(defun kbd-key:key-and-arguments (key-series)
  "Returns t if normalized KEY-SERIES appears to be a bound key sequence possibly with following interactive arguments, else nil."
  (let ((prefix-binding (and (stringp key-series) (key-binding (substring key-series 0 1)))))
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
  "Returns non-nil if normalized KEY-SERIES string is one of the following:
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

(provide 'hib-kbd)

;;; hib-kbd.el ends here
