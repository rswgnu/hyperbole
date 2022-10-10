;;; hact.el --- GNU Hyperbole button action handling  -*- lexical-binding: t; -let*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    18-Sep-91 at 02:57:09
;; Last-Mod:      7-Oct-22 at 23:01:56 by Mats Lidell
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

(eval-and-compile (mapc #'require '(hhist set)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hrule:action 'actype:act
  "Value is a function of any number of arguments that executes actions.
Variable is used to vary actual effect of evaluating a Hyperbole action,
e.g. to inhibit actions.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; ========================================================================
;;; symtable class - Hyperbole unordered symbol tables with fast lookup
;;; ========================================================================

(defvar symtable:category-plist nil
  "Property list mapping Hyperbole type category symbols to symtables.
The type categories are either `actypes' or `ibtypes'.")

(defsubst symtable:hash-table (symtable)
  "Return the hash-table containing symbol names and values from SYMTABLE."
  (plist-get symtable 'hash-table))

(defsubst symtable:name (symtable)
  "Return the name of SYMTABLE as a string."
  (plist-get symtable 'name))

(defsubst symtable:select (type-category)
  "Inline the return of the symtable for TYPE-CATEGORY.
TYPE-CATEGORY is one of `actypes' or `ibtypes'."
  (plist-get symtable:category-plist type-category))

(defun    symtable:operate (operation symbol-or-name symtable)
  "Call hash-table OPERATION with Hyperbole SYMBOL-OR-NAME key for SYMTABLE.
Trigger an error if SYMBOL-OR-NAME cannot be mapped to an existing Elisp
symbol or if SYMTABLE is invalid."
  (let ((name (cond ((stringp symbol-or-name)
		     symbol-or-name)
		    ((symbolp symbol-or-name)
		     (symbol-name symbol-or-name))
		    (t (error "(symtable:operate): Invalid type for symbol-or-name: %s" symbol-or-name))))
	(hash-table (plist-get symtable 'hash-table))
	(intern-op (if (eq operation #'puthash) #'intern #'intern-soft))
	def-name elisp-name elisp-symbol)
    (unless hash-table
      (error "(symtable:operate): symtable lacks required hash-table property: %s" symtable))
    (if (string-match "\\`\\(actypes\\|ibtypes\\)::" name)
	(setq def-name (substring name (match-end 0))
	      elisp-name name
	      elisp-symbol (funcall intern-op elisp-name))
      (setq def-name name
	    elisp-name (concat (symtable:name symtable) "::" name)
	    elisp-symbol (funcall intern-op elisp-name)))
    ;; Comment this out so can look for and try to remove symbols not yet defined.
    ;; (unless elisp-symbol
    ;;   (error "(symtable:operate): Use `%s' to create a new type named `%s' before using `%s' on it"
    ;; 	     (if (equal (plist-get symtable 'name) "actypes") "defact" "defib")
    ;; 	     def-name
    ;; 	     operation))
    (pcase operation
      ('gethash
       (funcall operation def-name   hash-table))
      ('remhash
       (funcall operation elisp-name hash-table)
       (funcall operation def-name   hash-table))
      ('puthash
       (funcall operation elisp-name elisp-symbol hash-table)
       (funcall operation def-name   elisp-symbol hash-table)
       (gethash def-name  hash-table))
      (_ (error "(symtable:operate): Invalid operation request: %s" operation)))))

(defun    symtable:create (name size)
  "Create and return a new Hyperbole type symbol table with NAME and SIZE.
Also add it under the symbol for its NAME in `symtable:category-plist'."
  (let ((symtable (list 'name name
			'hash-table (make-hash-table :test #'equal :size size))))
    (setq symtable:category-plist (plist-put symtable:category-plist (intern name) symtable))
    symtable))

(defvar   symtable:actypes (symtable:create "actypes" 97)
  "Symbol table (hash table) of Hyperbole action type symbols.
For each actype, there are two entries whose keys are strings: one
with the `actypes::' prefix and one without.  The value for both
keys is the Elisp symbol for the type, which includes the prefix.")

(defvar   symtable:ibtypes (symtable:create "ibtypes" 97)
  "Symbol table (hash table) of Hyperbole implicit button type symbols.
For each ibtype, there are two entries whose keys are strings: one
with the `ibtypes::' prefix and one without.  The value for both
keys is the Elisp symbol for the type, which includes the prefix.")

(defsubst symtable:actype-p (symbol-or-name)
  "Return SYMBOL-OR-NAME if it is a Hyperbole action type, else nil."
  (when (or (symbolp symbol-or-name) (stringp symbol-or-name))
    (symtable:get symbol-or-name symtable:actypes)))

(defsubst symtable:ibtype-p (symbol-or-name)
  "Return SYMBOL-OR-NAME if it is a Hyperbole implicit button type, else nil."
  (when (or (symbolp symbol-or-name) (stringp symbol-or-name))
    (symtable:get symbol-or-name symtable:ibtypes)))

(defun    symtable:add (symbol-or-name symtable)
  "Add Hyperbole SYMBOL-OR-NAME to existing SYMTABLE.
Return the Elisp symbol for SYMBOL-OR-NAME.
Caller must ensure SYMBOL-OR-NAME is a symbol or string."
  (symtable:operate #'puthash symbol-or-name symtable))

(defalias 'symtable:delete #'symtable:remove)

(defun    symtable:get (symbol-or-name symtable)
  "Return Hyperbole SYMBOL-OR-NAME if it is in SYMTABLE, else nil.
Caller must ensure SYMBOL-OR-NAME is a symbol or string."
  (symtable:operate #'gethash symbol-or-name symtable))

(defun    symtable:remove (symbol-or-name symtable)
  "Remove the Hyperbole SYMBOL-OR-NAME if it is in SYMTABLE.
Always return nil.
Caller must ensure SYMBOL-OR-NAME is a symbol or string."
  (symtable:operate #'remhash symbol-or-name symtable))


;;; ========================================================================
;;; symset class - Hyperbole internal ordered symbol sets
;;; ========================================================================

(defun    symset:create (symbol property &rest symbols)
  "Set SYMBOL's PROPERTY to a new symset created from any number of SyMBOLS.
If no SYMBOLS are given, set it to the empty set.  Return the symset.  Uses
`eq' for comparison."
  (let* ((set:equal-op 'eq)
	 (first (car symbols)))
    (when (and symbols first (listp first))
      (setq symbols first))
    (put symbol property (apply #'set:create symbols))))

(defun    symset:add (elt symbol property)
  "Add ELT to SYMBOL's PROPERTY set.
Return nil iff ELT is already in SET; otherwise, return PROPERTY's value.
Use `eq' for comparison."
  (let* ((set (get symbol property))
	 (set:equal-op 'eq)
	 (new-set (set:add elt set)))
    (and new-set (put symbol property new-set))))

(defun    symset:clear (symbol)
  "Set SYMBOL's symset to nil."
  (setf (symbol-plist symbol) nil))

(defalias 'symset:delete #'symset:remove)

(defun    symset:get (symbol property)
  "Return SYMBOL's PROPERTY set."
  (get symbol property))

(defun    symset:remove (elt symbol property)
  "Remove ELT from SYMBOL's PROPERTY set and return the new set.
Assume PROPERTY is a valid set.  Use `eq' for comparison."
  (let ((set (get symbol property))
	(set:equal-op 'eq))
    (put symbol property (set:remove elt set))))

;;; ========================================================================
;;; htype class - Hyperbole Types, e.g. action and implicit button types
;;; ========================================================================

(defun    htype:body (htype-sym)
  "Return body for HTYPE-SYM.  If HTYPE-SYM is nil, return nil."
  (and htype-sym (hypb:indirect-function htype-sym)))

(defun    htype:category (type-category)
  "Return list of symbols in Hyperbole TYPE-CATEGORY in priority order.
Symbols contain category component.
TYPE-CATEGORY should be `actypes', `ibtypes' or nil for all."
  (let ((def-symbols (symset:get type-category 'symbols))
	(symtable (symtable:select type-category)))
    ;; Expand def-symbols to Elisp symbols by adding prefix
    (when (and def-symbols symtable)
      (mapcar (lambda (sym) (symtable:get sym symtable)) def-symbols))))


;; Thanks to JWZ for help on this.
(defmacro htype:create (type type-category doc params body property-list)
  "Create a new Hyperbole TYPE within TYPE-CATEGORY (both unquoted symbols).
Third arg DOC is a string describing the type.
Fourth arg PARAMS is a list of parameters to send to the fifth arg BODY,
which is a list of forms executed when the type is evaluated.
Sixth arg PROPERTY-LIST is attached to the new type's symbol.

Return the new function symbol derived from TYPE."
  (when (null type)
    (error "(htype:create): `type' must not be null"))
  (let* ((sym (htype:symbol type type-category))
	 (action (nconc (list 'defun sym params doc) body)))
    `(progn
       ,action
       (setplist ',sym '(definition-name ,type ,@property-list))
       (symset:add ',type ',type-category 'symbols)
       (run-hooks 'htype-create-hook)
       ',sym)))

(defun    htype:def-symbol (type)
  "Return the abbreviated symbol used in the definition of a Hyperbole TYPE.
TYPE may be either an implicit button type or action type.  It may be
given as a string or a symbol."
  (let ((name (if (stringp type)
		  type
		(symbol-name type))))
    (when (string-match "\\`\\(ib\\|ac\\)types::" name)
      (make-symbol (substring name (match-end 0))))))

(defun    htype:delete (type type-category)
  "Delete a Hyperbole TYPE derived from TYPE-CATEGORY (both symbols).
Return the Hyperbole symbol for the TYPE if it existed, else nil."
  (let* ((sym (htype:symbol type type-category))
	 (exists (fboundp sym)))
    (setplist sym nil)
    (symtable:delete type (symtable:select type-category))
    (symset:delete type type-category 'symbols)
    (fmakunbound sym)
    (run-hooks 'htype-delete-hook)
    (and exists sym)))

(defun    htype:doc (type)
  "Return documentation for Hyperbole TYPE, a symbol."
  (documentation type))

(defun    htype:names (type-category &optional sym)
  "Return a list of current definition names for TYPE-CATEGORY in priority order.
Definition names do not contain the category prefix.
TYPE-CATEGORY should be `actypes', `ibtypes' or nil for all.
When optional SYM is given, returns the name for that symbol only, if any."
  (let ((types (symset:get type-category 'symbols))
	(sym-name (when sym (symbol-name sym))))
    (if sym-name
	;; Strip category from sym-name before looking for a match.
	(progn (when (string-match "::" sym-name)
		 (setq sym (make-symbol (substring sym-name (match-end 0)))))
	       (when (symtable:get sym (symtable:select type-category))
		 (symbol-name sym)))
      (mapcar #'symbol-name types))))

;;; ------------------------------------------------------------------------

(defun   htype:symbol (type type-category)
  "Return possibly new Hyperbole type symbol composed from TYPE and TYPE-CATEGORY.
TYPE and TYPE-CATEGORY are both symbols.  TYPE-CATEGORY must be one of
`actypes' or `ibtypes'; if not, return nil."
  (when (memq type-category '(actypes ibtypes))
    (intern (concat (symbol-name type-category) "::" (symbol-name type)))))

;;; ========================================================================
;;; action class
;;; ========================================================================

(defun action:commandp (function)
  "Return interactive calling form if FUNCTION has one, else nil."
  (let ((action
	 (cond ((null function) nil)
	       ((symbolp function)
		(and (fboundp function)
		     (hypb:indirect-function function)))
	       ((and (listp function)
		     (eq (car function) 'autoload))
		(error "(action:commandp): Autoload not supported: %s" function))
	       (t function))))
    (cond ((and action (fboundp 'interactive-form))
	   (interactive-form action))
	  ((byte-code-function-p action)
	   (cond ((fboundp 'compiled-function-interactive)
		  (compiled-function-interactive action))
		 ((commandp action)
		  (list 'interactive (aref action 5)))))
	  (t (commandp action)))))

(defun action:create (param-list body)
  "Create Hyperbole action defined by PARAM-LIST and BODY, a list of Lisp forms."
  (if (symbolp body)
      body
    (list 'function (cons 'lambda (cons param-list body)))))

(defun action:kbd-macro (macro &optional repeat-count)
  "Return Hyperbole action that execute a keyboard MACRO REPEAT-COUNT times."
  (list 'execute-kbd-macro macro repeat-count))

(defun action:params-emacs (def)
  "Return the argument list for the function DEF.
DEF may be a symbol or a function body."
  (let ((params (help-function-arglist def t)))
    (cond ((listp params) ;; includes nil
	   params)
	  ((stringp params)
	   (when (and (autoloadp def) (not (eq (nth 4 def) 'keymap)))
	     ;; Force autoload to get function signature.
	     (setq def (autoload-do-load def))
	     (unless (autoloadp def)
	       (action:params-emacs def))))
	  (t
	   (error "(action:params-emacs): Construct not supported: %s" def)))))

(defun action:params (action)
  "Return unmodified ACTION parameter list.
Autoloads action function if need be to get the parameter list."
  (when (and (symbolp action) (fboundp action))
    (setq action (hypb:indirect-function action)))
  (cond ((null action) nil)
	((fboundp 'help-function-arglist)
	 (help-function-arglist action t))
	((listp action)
	 (cond ((eq (car action) 'closure)
		(nth 2 action))
	       ((eq (car action) 'autoload)
		(error "(action:params): Autoload not supported: %s" action))
	       (t (car (cdr action)))))
	((byte-code-function-p action)
	 (if (fboundp 'compiled-function-arglist)
	     (compiled-function-arglist action)
	   (action:params-emacs action)))
	((symbolp action)
	 (car (cdr (and (fboundp action) (hypb:indirect-function action)))))))

(defun action:param-list (action)
  "Return list of actual ACTION parameters (remove `&' special forms)."
  (delq nil (mapcar (lambda (param)
		      (if (eq (aref (symbol-name param) 0) ?&)
			  nil param))
	      (action:params action))))

;;; ========================================================================
;;; action type class, actype
;;; ========================================================================

(defun hact (&rest args)
  "Perform action formed from rest of ARGS and return the result.
The value of `hrule:action' determines what effect this has.
Alternatively act as a no-op when testing implicit button type contexts.
First arg may be a symbol or symbol name for either an action type or a
function.  Runs `action-act-hook' before performing action."
  (apply hrule:action args))

(defun    actype:act (actype &rest args)
  "Perform action formed from ACTYPE and rest of ARGS and return value.
If value is nil, however, t is returned instead, to ensure that implicit button
types register the performance of the action.  ACTYPE may be a symbol or symbol
name for either an action type or a function.  Runs `action-act-hook' before
performing ACTION."
  (when (null actype)
    (error "(actype:act): No action type specified"))
  (let ((prefix-arg current-prefix-arg)
	(action (actype:action actype)))
    (if (null action)
	(error "(actype:act): Null action for action type: `%s'" actype)
      ;; Next 2 lines are needed so that relative paths are expanded
      ;; properly.  But in rare cases, this can improperly expand simple
      ;; string arguments like "tags" as a pathname, when it is not
      ;; being used as a path.  So do this only if actype is a defact
      ;; and not a defun to limit any potential impact. RSW - 9/22/2017
      (and (symbolp action)
	   (symtable:actype-p action)
	   (setq args (hpath:absolute-arguments actype args)))
      (let ((hist-elt (hhist:element)))
	(run-hooks 'action-act-hook)
	(prog1 (or (if (or (symbolp action) (listp action)
			   (byte-code-function-p action)
			   (subrp action)
			   (and (stringp action) (not (integerp action))
				(setq action (key-binding action))))
		       (eval (cons action args))
		     (eval action))
		   t)
	  (hhist:add hist-elt))))))

;; Return the full Elisp symbol for ACTYPE, which may be a string or symbol.
(defalias 'actype:elisp-symbol #'symtable:actype-p)

(defun    actype:def-symbol (actype)
  "Return the abbreviated symbol for ACTYPE used in its `defact'.
ACTYPE must be a symbol or string that begins with `actype::' or nil
is returned."
  (let ((name (if (stringp actype)
		  actype
		(symbol-name actype))))
    (when (string-match "\\`actypes::" name)
      (make-symbol (substring name (match-end 0))))))

(defun    actype:eval (actype &rest args)
  "Perform action formed from ACTYPE and rest of ARGS and return value.
ACTYPE may be a string containing a Lisp expression from which ACTYPE
and ARGS are extracted.  ACTYPE may be a symbol or symbol name for
either an action type or a function.  Run `action-act-hook' before
performing ACTION."
  (let ((prefix-arg current-prefix-arg)
	(action (actype:action actype)))
    (if (null action)
	(error "(actype:act): Null action for: `%s'" actype)
      (let ((hist-elt (hhist:element)))
	(run-hooks 'action-act-hook)
	(prog1 (if (or (symbolp action) (listp action)
		       (byte-code-function-p action)
		       (subrp action)
		       (and (stringp action) (not (integerp action))
			    (setq action (key-binding action))))
		   (apply action args)
		 (eval action))
	  (hhist:add hist-elt))))))

(defun    actype:action (actype)
  "Return action part of ACTYPE.
ACTYPE is a bound function symbol, symbol name or function body.
ACTYPE may be a Hyperbole actype or Emacs Lisp function."
  (let (actname
	action)
    (cond ((stringp actype)
	   (setq actname actype
		 actype (intern actype)))
	  ((and actype (symbolp actype))
	   (setq actname (symbol-name actype))))
    (setq actype (or (symtable:actype-p actname) actype)
	  action (htype:body actype))
    (if (functionp actype)
	actype
      action)))

(defun    actype:action-body (actype)
  "Return action body derived from ACTYPE (a symbol or symbol name).
ACTYPE may be a Hyperbole actype or Emacs Lisp function.
If no action body and actype is a bound function symbol, return that."
  (let (actname)
    (if (stringp actype)
	(setq actname actype
	      actype (intern actype))
      (setq actname (symbol-name actype)))
    (cond ((htype:body (or (symtable:actype-p actname) actype)))
	  ((fboundp actype) actype))))

(defmacro defact (type params doc &rest default-action)
  "Create an action TYPE (an unquoted symbol) with PARAMS, described by DOC.
The type uses PARAMS to perform DEFAULT-ACTION (list of the rest of the
arguments).  A call to this function is syntactically the same as for
`defun',  but a doc string is required.
Return symbol created when successful, else nil."
  `(progn
     (symtable:add ',type symtable:actypes)
     (htype:create ,type actypes ,doc ,params ,default-action nil)))

(put      'defact 'lisp-indent-function 'defun)

;; Support edebug-defun for interactive debugging of actypes
(def-edebug-spec defact
  (&define name lambda-list
           [&optional stringp]   ; Match the doc string, if present.
           def-body))

(def-edebug-spec lambda-list
  (([&rest arg]
    [&optional ["&optional" arg &rest arg]]
    &optional ["&rest" arg])))

(defalias 'actype:create #'defact)

(defun    actype:delete (type)
  "Delete an action TYPE (a symbol).  Return TYPE's symbol if it existed."
  (symtable:delete type symtable:actypes)
  (htype:delete type 'actypes))

(defun    actype:doc (hbut &optional full)
  "Return first line of act doc for HBUT (a Hyperbole button symbol).
With optional FULL, returns full documentation string.
Return nil when no documentation."
  (let* ((act (and (hbut:is-p hbut) (or (hattr:get hbut 'action)
					(hattr:get hbut 'actype))))
	 (but-type (hattr:get hbut 'categ))
	 (sym-p (and act (symbolp act)))
	 (end-line) (doc))
    (cond ((and (functionp but-type)
		(setq doc (htype:doc but-type)))
	   ;; Is an implicit button, so use its doc string if any.
	   )
	  (sym-p
	   (setq doc (htype:doc act))))
    (when doc
      (setq doc (substitute-command-keys doc))
      (or full (setq end-line (string-match "[\n]" doc)
		     doc (substring doc 0 end-line))))
    doc))

(defun    actype:identity (&rest args)
  "Return list of ARGS unchanged or if no ARGS, return t.
Used as the setting of `hrule:action' to inhibit action evaluation."
  (or args t))

(defun    actype:interact (actype)
  "Interactively call default action for ACTYPE.
ACTYPE is a symbol that was previously defined with `defact'.
Return nil only when no action is found or the action has no interactive
calling form."
  (let ((action (htype:body (symtable:actype-p actype))))
    (and action (action:commandp action) (or (call-interactively action) t))))

(defun    actype:params (actype)
  "Return list of ACTYPE's parameters, including keywords."
  (action:params (actype:action actype)))

(defun    actype:param-list (actype)
  "Return list of ACTYPE's parameters without keywords."
  (action:param-list (actype:action actype)))

(provide 'hact)

;;; hact.el ends here
