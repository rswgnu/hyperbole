;;; hyperbole.el --- GNU Hyperbole: The Everyday Hypertextual Information Manager  -*- lexical-binding: t; -*-

;; Copyright (C) 1992-2021  Free Software Foundation, Inc.

;; Author:           Bob Weiner
;; Maintainer:       Bob Weiner <rsw@gnu.org>, Mats Lidell <matsl@gnu.org>
;; Created:          06-Oct-92 at 11:52:51
;; Released:         03-May-21
;; Version:          8.0.0pre
;; Keywords:         comm, convenience, files, frames, hypermedia, languages, mail, matching, mouse, multimedia, outlines, tools, wp
;; Package:          hyperbole
;; Package-Requires: ((emacs "24.4"))
;; URL:              http://www.gnu.org/software/hyperbole

;; See the "HY-COPY" file for license information.

;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; GNU Hyperbole (pronounced Gnew Hi-per-bo-lee), or just Hyperbole, is like
;; Markdown for hypertext, letting you use little or no markup in files to
;; produce easy-to-navigate hypermedia files.  It is an easy-to-use, yet
;; powerful and programmable hypertextual information management system
;; implemented as a GNU Emacs package.  It offers rapid views and interlinking
;; of all kinds of textual information, utilizing Emacs for editing.  It can
;; dramatically increase your productivity and greatly reduce the number of
;; keyboard/mouse keys you'll need to work efficiently.
;; 
;; Hyperbole lets you:
;; 
;; 1. Quickly create hyperlink buttons either from the keyboard or by dragging
;; between a source and destination window with a mouse button depressed.
;; Later activate buttons by pressing/clicking on them or by giving the name of
;; the button.
;; 
;; 2. Activate many kinds of `implicit buttons' recognized by context within
;; text buffers, e.g. URLs, grep output lines, and git commits.  A single key
;; or mouse button automatically does the right thing in dozens of contexts;
;; just press and go.
;; 
;; 3. Build outlines with multi-level numbered outline nodes, e.g. 1.4.8.6,
;; that all renumber automatically as any node or tree is moved in the
;; outline.  Each node also has a permanent hyperlink anchor that you can
;; reference from any other node;
;; 
;; 4. Manage all your contacts quickly with hierarchical categories and embed
;; hyperlinks within each entry.  Or create an archive of documents with
;; hierarchical entries and use the same search mechanism to quickly find any
;; matching entry;
;; 
;; 5. Use single keys to easily manage your Emacs windows or frames and quickly
;; retrieve saved window and frame configurations;
;; 
;; 6. Search for things in your current buffers, in a directory tree or across
;; major web search engines with the touch of a few keys.
;; 
;; The common thread in all these features is making retrieval, management and
;; display of information fast and easy.  That is Hyperbole's purpose.
;; 
;; ----
;;
;; See the "INSTALL" file for installation instructions and the "README" file
;; for general information.
;;
;; There is no need to manually edit this file unless there are specific
;; customizations you would like to make, such as whether a Hyperbole Action
;; Mouse Key is bound to the middle mouse button.  (See the call of the
;; function, `hmouse-install', below).
;;
;; Other site-specific customizations belong in "hsettings.el".

;;; Code:
;;; ************************************************************************
;;; Start Initializations
;;; ************************************************************************

(defconst hyperbole-loading t
  "Temporary constant available for testing while Hyperbole is loading.")

;; Ensure defgroup and defcustom are defined for use throughout Hyperbole.
(require 'custom)

(defgroup hyperbole nil
  "Hyperbole customizations category."
  :group 'applications)

(defgroup hyperbole-buttons nil
  "Hyperbole explicit, global and implicit button customizations."
  :group 'hyperbole)

(defgroup hyperbole-commands nil
  "Hyperbole command customizations."
  :group 'hyperbole)

(defgroup hyperbole-keys nil
  "Hyperbole keyboard and mouse key customizations."
  :group 'hyperbole)

;; defgroup hyperbole-rolo is in "hyrolo.el".

(defgroup hyperbole-screen nil
  "Hyperbole screen/display customizations, typically frame or window-related."
  :group 'hyperbole)

;; Reinitialize hyperb:dir on reload if initialization failed for any reason.
(eval-and-compile
  (when (and (boundp 'hyperb:dir) (null hyperb:dir))
    (makunbound 'hyperb:dir)
    (setq features (delq 'hload-path features)
	  features (delq 'hversion features)))

  ;; Defines hyperb:path-being-loaded, hyperb:stack-frame,
  ;; (hyperb:window-system) and hyperb:dir, which are used later in
  ;; this file.  Also adds Hyperbole to the load-path if need be.
  ;;
  ;; This handles the case when the Hyperbole package directory is not yet in load-path.
  (unless (or (require 'hversion nil t)
	      (and (stringp load-file-name)
		   (require 'hversion (expand-file-name
				       "hversion"
				       (file-name-directory load-file-name))
			    t)))
    (error "(Hyperbole): Startup failure: `hyperb:dir' must be manually added to `load-path' to fix")))

(defgroup hyperbole-koutliner nil
  "Hyperbole multi-level autonumbered outliner customizations."
  :group 'hyperbole)

(defvar hyperbole-mode-map (make-sparse-keymap)
  "Keymap for the GNU Hyperbole global minor mode.
See `hkey-initialize'.")

(defcustom hyperbole-mode-lighter " Hypb"
  "Text to display in the minor-mode area of the modeline when the Hyperbole global minor mode is active."
  :type 'string
  :group 'hyperbole)

;;;###autoload
(define-minor-mode hyperbole-mode
  "Toggle the Everyday Hypertextual Information Manager global minor mode (Hyperbole mode).

When Hyperbole mode is enabled, the `hyperbole-mode' variable
is non-nil, Hyperbole menus are enabled, as are Hyperbole keys.

Invoke the Hyperbole minibuffer menu with \\[hyperbole].  See the
documentation at \"(hyperbole)Top\".

\\{hyperbole-mode-map}"
  :global t
  :lighter hyperbole-mode-lighter
  (if hyperbole-mode
      (hyperbole--enable-mode)
    (hyperbole--disable-mode)))

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;; Avoid any potential library name conflict by giving the load directory.
(require 'set (expand-file-name "set" hyperb:dir))

(require 'hui-select)  ;; This requires 'hvar which defines the var:append function.

;;; ************************************************************************
;;; Public Variables
;;; ************************************************************************

(defcustom hkey-init t
  "*A non-nil value (default) at system load time binds the Action and Assist Keyboard Keys, as well as other keys.
{\\[hkey-either]} invokes the Action Key and {C-u \\[hkey-either]} invokes the Assist Key.
Additionally, {\\[hkey-help]} shows what the Action Key will do in the current
context (wherever point is).  {C-u \\[hkey-help]} shows what the Assist Key will do."
  :type 'boolean
  :group 'hyperbole-keys)

;;; ************************************************************************
;;; Public key bindings
;;; ************************************************************************

;;
;; Hyperbole key binding for many read-only modes.  Set to nil if unwanted.
;; No longer used; use regular Action Key instead.
;;(defvar action-key-read-only "\C-m"
;;  "Local Action Key binding for special read-only modes.  Set to nil if unwanted.")

(defvar hkey-bindings nil
  "List of global key sequences bound by Hyperbole.
See `hkey-binding-entry' for format.")

(defvar hkey-bindings-flag nil
  "True if Hyperbole key bindings are in use, else nil.")

(defvar hkey-previous-bindings nil
  ;; !! FIXME: We should probably instead use a keymap that we just add/remove
  ;; from the `global-map' so we don't actually modify any keybindings (and
  ;; hence don't need this var)?
  "List of bindings that Hyperbole has overridden.
Holds the global key sequences and their pre-Hyperbole bindings.
See `hkey-binding-entry' for format.")

(defun hkey-binding-entry (key)
  "Given an Emacs KEY that may be bound, return an entry to save the associated binding.
Entry format is: (key-description key-sequence key-binding)."
  (list (key-description key) key (key-binding key)))

(defun hkey-bindings-keys (entries)
  (mapcar #'cadr entries))

(defun hkey-get-bindings ()
  "Return a list of entries for storage of Hyperbole key bindings.
`hkey-initialize' must have already been called or the list will be empty."
  (mapcar (lambda (key) (hkey-binding-entry key))
	  (hkey-bindings-keys hkey-previous-bindings)))

(defun hkey-global-set-key (key command &optional no-add)
  "Same as `global-set-key' except saves prior binding for later restoration unless optional 3rd argument NO-ADD is given as a non-nil value."
  (unless no-add
    (add-to-list 'hkey-previous-bindings (hkey-binding-entry key)))
  (global-set-key key command))

(defun hkey-maybe-global-set-key (key command &optional no-add)
  "Globally set KEY to COMMAND if KEY is unbound and COMMAND is not on any global key.
With third argument NO-ADD non-nil, skip storage of prior KEY binding
which prevents automatic removal of any local bindings to the same key."
  (or (global-key-binding key)
      (where-is-internal command)
      (hkey-global-set-key key command no-add)))

(defvar hmouse-middle-flag)
(defvar hmouse-bindings-flag)
(defvar hyperb:user-email)
;; (defvar hyperbole-help-map (make-sparse-keymap)
;;  "Help prefix keymap available only when Hyperbole minor mode is enabled.")
;; (defvar hyperbole-mode-specific-map (make-sparse-keymap)
;;   "C-c prefix keymap available only when Hyperbole minor mode is enabled.")

(defun hkey-initialize ()
  "If `hkey-init' is non-nil, initialize Hyperbole key bindings.
Some keys are conditionally bound only if there are no existing prior bindings
of the commands."
  (when hkey-init
    ;;
    ;; Setup so Hyperbole menus can be accessed from a key.  If not
    ;; already bound to a key, this typically binds the command `hyperbole'
    ;; globally to {C-h h} and activates Hyperbole minor mode.
    (or (where-is-internal #'hyperbole)
	;; In GNU Emacs, this binding replaces a command that shows
	;; the word hello in foreign languages; this binding makes this
	;; key much more useful.
	(global-set-key (vector help-char ?h) #'hyperbole))
    ;;
    ;; Define help prefix key in this keymap.
    ;; (set-keymap-parent hyperbole-help-map help-map)
    ;; (hkey-set-key (vector help-char) hyperbole-help-map)
    ;;
    ;; Define C-c prefix key in this keymap.
    ;; (set-keymap-parent hyperbole-mode-specific-map mode-specific-map)
    ;; (hkey-set-key "\C-c" hyperbole-mode-specific-map)
    ;;
    ;; Binds the Action Key to {M-RET} and the Assist Key to {C-u M-RET}
    ;; and loads the Hyperbole mouse key bindings.
    (unless (where-is-internal #'hkey-either)
      ;; Need to map all these variants to ensure can override
      ;; org-meta-return in Org mode when desired.
      (mapc (lambda (key) (hkey-set-key (kbd key) #'hkey-either))
	    '("\M-\C-m" "M-<return>" "M-RET" "ESC <return>" "ESC RET")))
    ;;
    ;; Typically bind the key, {C-h A}, for Action Key help and {C-u C-h A} for Assist key
    ;; help.
    (or (where-is-internal #'hkey-help)
	(hkey-set-key (vector help-char ?A) #'hkey-help))
    ;;
    ;; Provides a site standard way of emulating most Hyperbole mouse drag
    ;; commands from the keyboard.  This is most useful for rapidly creating
    ;; Hyperbole link buttons from the keyboard without invoking the Hyperbole
    ;; menu.  Only works if Hyperbole is run under a window system.
    (when (hyperb:window-system)
      (if (eq (global-key-binding "\M-o") 'facemenu-keymap)
	  ;; Override facemenu package that adds a keymap on M-o,
	  ;; since this binding is more important to Hyperbole
	  ;; users.
	  (hkey-set-key "\M-o" #'hkey-operate)
	(hkey-maybe-set-key "\M-o" #'hkey-operate)))
    ;;
    ;; Binds {C-c @} to create a user-specified sized grid of windows
    ;; displaying different buffers.
    ;;
    ;; Don't override prior bindings of this key.
    (hkey-maybe-set-key "\C-c@" #'hycontrol-windows-grid)
    ;;
    ;; Explicit button renames without invoking the Hyperbole menu.
    ;; No binding by default.
    ;; Don't override prior bindings of this key.
    ;; (hkey-maybe-set-key "\C-cr" #'hui:ebut-rename)
    ;;
    ;; Binds {C-c RET} to select larger and larger syntactical units in a
    ;; buffer when invoked repeatedly, showing in the minibuffer the type
    ;; of unit selected each time.
    (hkey-maybe-set-key "\C-c\C-m" #'hui-select-thing)
    ;;
    ;; Binds {C-c \} to interactively manage windows and frames.
    (hkey-maybe-set-key "\C-c\\" #'hycontrol-enable-windows-mode)
    ;;
    ;; Binds {C-c /} to display the Hyperbole Find/Web search menu.
    (hkey-maybe-set-key "\C-c/" #'hui-search-web)
    ;;
    ;; Binds {C-c .} to jump between the start and end of a delimited thing.
    ;; Don't override prior bindings of this key.
    (hkey-maybe-set-key "\C-c." #'hui-select-goto-matching-delimiter)
    ;;
    ;; This initializes the Smart Mouse Key bindings.  Shifted mouse buttons
    ;; are always set up.  Under InfoDock or with `hmouse-middle-flag'
    ;; non-nil, this also binds the middle mouse button to the Action Key.
    ;; These bindings are ignored if a particular frame does not have mouse
    ;; support.
    (hmouse-install hmouse-middle-flag)
    ;;
    ;; This makes a double or triple click of the left mouse button do the
    ;; same thing as {C-c RET}.  It also sets up Java, C++ and HTML modes
    ;; for proper entity selection.
    (hui-select-initialize)
    ;;
    ;; Store Hyperbole key bindings so can turn them on and off.
    (setq hkey-bindings (hkey-get-bindings)
	  hkey-bindings-flag t)))

(defun hkey-set-bindings (key-binding-list)
  "Set keys bound by Hyperbole to those in KEY-BINDING-LIST.
KEY-BINDING-LIST is the value of either `hkey-previous-bindings'
\(key bindings prior to Hyperbole load) or `hkey-bindings' (Hyperbole
bindings after load)."
  (dolist (key-and-binding key-binding-list)
     (global-set-key (cadr key-and-binding) (car (cddr key-and-binding)))))

(defun hyperbole-toggle-bindings ()
  "Toggle between Hyperbole mouse and keyboard keys and their prior bindings."
  (interactive)
  (let ((key-binding-list (if hkey-bindings-flag
			      hkey-previous-bindings
			    hkey-bindings))
	(other-bindings-var (if hkey-bindings-flag
				'hkey-bindings
			      'hkey-previous-bindings)))
    (if key-binding-list
	(progn
	  (set other-bindings-var (hkey-get-bindings))
	  (hkey-set-bindings key-binding-list)
	  (setq hkey-bindings-flag (not hkey-bindings-flag)
		hmouse-bindings-flag (not hmouse-bindings-flag))
	  (if (called-interactively-p 'interactive)
	      (message "%s mouse and keyboard bindings are now in use."
		       (if hkey-bindings-flag "Hyperbole" "Non-Hyperbole"))))
      (error "(hyperbole-toggle-bindings): `%s' is empty"
	     (if hkey-bindings-flag 'hkey-previous-bindings 'hkey-bindings)))))

;;; ************************************************************************
;;; Load Hyperbole mouse bindings
;;; ************************************************************************

;; From mouse-position.c in Emacs:
;;    f = SELECTED_FRAME ();
;;    XSETFRAME (lispy_dummy, f);
;;
;;  It seems like the XSETFRAME macro is not properly copying the value of f on initial frame selection under the macOS window system.
;;  The problem occurs on other systems as well, e.g. Emacs 25.2 under Windows 7.
;;
;;  Hyperbole resolves this problem by setting the
;;  `mouse-position-function' variable below to properly set the
;;  newly selected frame.
(if (boundp 'mouse-position-function)
    (setq mouse-position-function
	  (lambda (frame-x-dot-y)
	    "Make `mouse-position' and `mouse-pixel-position' always return the selected frame.
Under macOS and Windows 7 at least, upon initial selection of a new
frame, those functions by default still return the prior frame."
	    (if (consp frame-x-dot-y) (setcar frame-x-dot-y (selected-frame)))
	    frame-x-dot-y)))

;; hmouse-drv will load hui-mouse and hmouse-key
(mapc #'require '(hsettings hmouse-drv hmouse-sh))

;;; ************************************************************************
;;; You shouldn't need to modify anything below here.
;;; ************************************************************************

;; Add Hyperbole Info directory to Info-directory-list after the Info
;; manual reader package is loaded.
(eval-after-load "info"
  '(when (boundp 'hyperb:dir)
     (info-initialize)
     (let ((info-dir (expand-file-name "man/" hyperb:dir)))
       (if (file-exists-p info-dir)
	   (add-to-list 'Info-directory-list info-dir)))))

;;; ************************************************************************
;;; Display Hooks
;;; ************************************************************************

;; Permits restore of the prior window configuration after any help buffer
;; is shown by pressing either the Action or Assist Key at the end of the
;; help buffer.  (Help buffer names end with "Help*".)  Only one of
;; these two settings is used, dependent on emacs version.
;;
(add-hook 'temp-buffer-show-hook #'hkey-help-show)
(setq temp-buffer-show-function #'hkey-help-show)

;;; ************************************************************************
;;; Autoloads
;;; ************************************************************************

;; New autoload generation function defined only in Emacs 28
(unless (fboundp #'make-directory-autoloads)
  (defun make-directory-autoloads (dir output-file)
    "Update autoload definitions for Lisp files in the directories DIRS.
DIR can be either a single directory or a list of
directories.  (The latter usage is discouraged.)

The autoloads will be written to OUTPUT-FILE.  If any Lisp file
binds ‘generated-autoload-file’ as a file-local variable, write
its autoloads into the specified file instead.

The function does NOT recursively descend into subdirectories of the
directory or directories specified."
    ;; Don't use a 'let' on this next line or it will fail.
    (setq generated-autoload-file output-file)
    (with-suppressed-warnings ((obsolete update-directory-autoloads))
      (update-directory-autoloads dir))))

;; Before the 6.0.1 release, Hyperbole used to patch the package-generate-autoloads
;; function to ensure that kotl/ subdirectories were autoloaded.  This
;; is no longer used but is left here temporarily for reference.
;;
;; Ensure Koutliner autoloads in kotl/ subdirectory are generated and loaded.
;; (unless (or (fboundp 'kotl-mode)
;; 	    (and (load "hyperbole-autoloads" t t)
;; 		 (fboundp 'kotl-mode)))
;;   (defun hyperb:package-autoloads-subdirectories-p ()
;;     (require 'package)
;;     (let ((func (symbol-function 'package-generate-autoloads)))
;;       ;; If this function contains a call to apply, then it is patched
;;       ;; with support for finding autoloads in subdirectories and
;;       ;; nothing more need be done.
;;       (if (byte-code-function-p func)
;; 	  (delq nil (mapcar (lambda (item) (eq item 'apply)) (aref func 2)))
;; 	(string-match "(apply " (prin1-to-string func)))))

;;   (unless (hyperb:package-autoloads-subdirectories-p)
;;     ;; Function is not patched, so define it here, call it, and then load
;;     ;; the generated autoloads.  This will happen maximally only once
;;     ;; per installation of a Hyperbole release.
;;     (if (not (file-writable-p (expand-file-name "hyperbole-autoloads.el" hyperb:dir)))
;; 	(error "(Hyperbole): Failure loading, need write permission to \"%s\"" hyperb:dir))
;;     (defun package-generate-autoloads (name pkg-dir)
;;       (let* ((auto-name (format "%s-autoloads.el" name))
;; 	     (generated-autoload-file (expand-file-name auto-name pkg-dir))
;; 	     ;; Silence `autoload-generate-file-autoloads'.
;; 	     (noninteractive t)
;; 	     (backup-inhibited t)
;; 	     (version-control 'never))
;; 	(package-autoload-ensure-default-file generated-autoload-file)
;; 	(apply #'update-directory-autoloads pkg-dir
;; 	       (delq nil (mapcar (lambda (f) (and (file-directory-p f)
;; 						  (not (file-symlink-p f))
;; 						  f))
;; 				 (directory-files pkg-dir t "[a-zA-Z].*" nil))))
;; 	(let ((buf (find-buffer-visiting generated-autoload-file)))
;; 	  (when buf (kill-buffer buf)))
;; 	auto-name))
;;     (package-generate-autoloads "hyperbole" hyperb:dir))
;;   (load "hyperbole-autoloads"))

;; Menu items could call this function before Info is loaded.
(autoload 'Info-goto-node   "info"       "Jump to specific Info node."  t)

;; Hyperbole user interface entry points that trigger loading of the
;; full Hyperbole system.  These are left commented here for
;; reference in case we ever go back to autoloading Hyperbole rather
;; than initializing it fully in this file.

;; ;; Action type definitions.
;; (autoload 'defact            "hyperbole"
;;   "Creates an action TYPE (an unquoted symbol) with PARAMS, described by DOC."
;;   nil 'macro)
;; ;; Implicit button type definitions.
;; (autoload 'defib             "hyperbole"
;;   "Creates implicit button TYPE (unquoted sym) with PARAMS, described by DOC."
;;   nil 'macro)

;; (autoload 'ebut:map          "hyperbole"      "Map over the Hyperbole explicit buttons in a buffer." nil)
;; (autoload 'hbut:key-src      "hyperbole"      "Called by {e} command in rolo match buffer.")
;; (autoload 'hui:ebut-rename   "hyperbole"      "Rename a Hyperbole button."     t)
;; (autoload 'hyperbole         "hyperbole"      "Hyperbole info manager menus."  t)

;; (autoload 'action-key        "hyperbole"
;;   "Context-sensitive Action Key command."                                  t)
;; (autoload 'action-key-depress "hyperbole"     "Depress context-sensitive Action Key." t)
;; (autoload 'assist-key-depress "hyperbole"     "Depress context-sensitive Assist Key." t)
;; (autoload 'action-key-depress-emacs "hyperbole" "Depress context-sensitive Action Key." t)
;; (autoload 'assist-key-depress-emacs "hyperbole" "Depress context-sensitive Assist Key." t)
;; (autoload 'action-mouse-key-emacs  "hyperbole" "Execute context-sensitive Action Key." t)
;; (autoload 'assist-mouse-key-emacs  "hyperbole" "Execute context-sensitive Assist Key." t)
;; (autoload 'hkey-help         "hyperbole"
;;   "Display help for the Action Key command in current context.
;; With optional ASSIST-FLAG non-nil, display help for the Assist Key command.
;; Returns non-nil iff associated help documentation is found."               t)
;; (autoload 'hkey-assist-help  "hyperbole"
;;   "Display help for the Assist Key command in current context."            t)
;; (autoload 'hkey-help-hide    "hyperbole"
;;   "Restores frame to configuration prior to help buffer display."        nil)
;; (autoload 'hkey-help-show    "hyperbole"
;;   "Saves prior frame configuration if BUFFER displays help."             nil)
;; (autoload 'assist-key        "hyperbole"
;;   "Context-sensitive Assist Key command."                                  t)
;; (autoload 'action-mouse-key  "hyperbole"
;;   "Context-sensitive Action Mouse Key command."                            t)
;; (autoload 'assist-mouse-key  "hyperbole"
;;   "Context-sensitive Assist Mouse Key command."                            t)
;; (autoload 'hkey-operate      "hyperbole"      "Emulate Hyperbole mouse key drags." t)
;; (autoload 'symset:add        "hyperbole"      "Adds ELT to SYMBOL's PROP set." nil)
;; (autoload 'hact              "hyperbole"      "Performs action formed from rest of ARGS." nil)
;; (autoload 'actypes::exec-window-cmd "hyperbole"
;; 	  "Executes an external window-based SHELL-CMD string asynchronously." nil)
;; (autoload 'hpath:absolute-to "hyperbole"
;; 	  "Make PATH absolute from optional DEFAULT-DIRS." nil)
;; (autoload 'hpath:display-buffer "hyperbole"
;; 	  "Displays and selects BUFFER at optional DISPLAY-WHERE location or at `hpath:display-where'." t)
;; (autoload 'hpath:find        "hyperbole"
;; 	  "Edit file FILENAME, possibly using a special command." t)
;; (autoload 'hpath:find-other-frame "hyperbole"
;; 	  "Edit file FILENAME in other frame, possibly using a special command." t)
;; (autoload 'hpath:find-other-window "hyperbole"
;; 	  "Edit file FILENAME in other window, possibly using a special command." t)

;; Auto-autoload doesn't work for next item because it is defined
;; within a condition-case, so autoload it here.
(autoload 'Vm-init    "hvm"    "Initializes Hyperbole Vm support." t)

;;; ************************************************************************
;;; Outline Mode Aliases
;;; ************************************************************************

(require 'outline)
(unless (fboundp 'outline-hide-body)
  (defalias 'outline-hide-body 'hide-body))
(unless (fboundp 'outline-hide-entry)
  (defalias 'outline-hide-entry 'hide-entry))
(unless (fboundp 'outline-show-entry)
  (defalias 'outline-show-entry 'show-entry))
(unless (fboundp 'outline-show-all)
  (defalias 'outline-show-all 'show-all))
(unless (fboundp 'outline-hide-subtree)
  (defalias 'outline-hide-subtree 'hide-subtree))
(unless (fboundp 'outline-show-subtree)
  (defalias 'outline-show-subtree 'show-subtree))
(unless (fboundp 'outline-flag-region)
  (defun outline-flag-region (from to flag)
    "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
    (if flag
	(subst-char-in-region from to ?\n ?\r t)
      (subst-char-in-region from to ?\r ?\n t))))
(unless (fboundp 'outline-invisible-in-p)
  (defun outline-invisible-in-p (beg end)
    "Return t if there is an invisible character between BEG and END, else nil."
    (catch 'result
      (let ((p beg))
	(while (< p end) 
	  (when (eq (get-char-property p 'invisible) 'outline)
	    (throw 'result t))
	  (setq p (1+ p))))
      nil)))

;;; ************************************************************************
;;; Message System Support Configuration
;;; ************************************************************************

;; Even if you don't need some of the following hook settings, you might
;; as well leave them in so that if they ever become useful to you, you
;; need not reconfigure Hyperbole.  These settings do nothing if the
;; corresponding subsystems are never invoked.
;;
;; GNUS USENET news reader/poster support.
;;
(var:append 'gnus-Startup-hook '(Gnus-init))
;;
;; Hyperbole mail reader support configuration.
;;
;; Rmail
(var:append 'rmail-mode-hook    '(Rmail-init))
;; Mh-e
(var:append 'mh-inc-folder-hook '(Mh-init))
;;
;; VM support is based on V5.72 beta of VM.  If you have a version of VM
;; earlier than 5.70 beta, you should either upgrade or comment out the
;; following line so that Hyperbole support for VM is not enabled.
(var:append 'vm-mode-hook       '(Vm-init))
;;
;; Hyperbole mail composer support configuration.
;;
(var:append 'message-mode-hook   (list (lambda () (require 'hsmail))))
(var:append 'mh-letter-mode-hook (list (lambda () (require 'hsmail))))
(var:append 'vm-mail-mode-hook   (list (lambda () (require 'hsmail))))


;;; ************************************************************************
;;; URL Browsing
;;; ************************************************************************

(require 'browse-url)

;; Use any obsolete URL setting from earlier Hyperbole releases to set the
;; new URL browsing variable.
(when (and (boundp 'action-key-url-function) action-key-url-function
	   (eq action-key-url-function #'eww))
  (setq browse-url-browser-function #'browse-url-emacs))

;;; ************************************************************************
;;; Load Site-specific Configurations and Initialize Hyperbole Package
;;; ************************************************************************

(defconst hyperb:cygdrive '("\\`/cygdrive/" . "/"))

(defun hyperb:init ()
  "Initialize standard Hyperbole configuration."
  (interactive)
  (message "Initializing Hyperbole...")
  ;;
  (run-hooks 'hyperbole-init-hook)
  (hyperb:check-dir-user)
  (unless (stringp hyperb:user-email)
    (setq hyperb:user-email
	  (or (and (boundp 'user-mail-address)
		   (stringp user-mail-address)
		   (string-match "@" user-mail-address)
		   user-mail-address)
	      (concat (user-login-name) (hypb:domain-name)))))
  ;;
  ;; When running from git source and not a release package, ensure
  ;; *-autoloads.el files are already generated or generate them.
  ;; Then ensure they are loaded.
  (unless noninteractive
    (hyperb:maybe-generate-autoloads))
  (hyperb:maybe-load-autoloads)
  ;;
  ;; Modify syntactic character pairs for use with implicit button activations.
  (hbut:modify-syntax)
  ;;
  ;; Conditionally initialize Hyperbole key bindings (when hkey-init is t).
  (hkey-initialize)
  ;;
  ;; The keymaps in `emulation-mode-map-alists' take precedence over
  ;; `minor-mode-map-alist'; add this only if other minor modes are
  ;; overriding Hyperbole keys.
  ;; (add-to-list 'emulation-mode-map-alists `((hyperbole-mode . ,hyperbole-mode-map)))
  ;;
  ;; Initialize this option after hyperbole-mode-map has been
  ;; initialized, if not yet set by the user.
  (when (eq hsys-org-enable-smart-keys 'unset)
    (customize-set-variable 'hsys-org-enable-smart-keys
			    (if (hsys-org-meta-return-shared-p)
				'buttons
			      t)))
  ;;
  ;; Hyperbole initialization is complete.
  (message "Initializing Hyperbole...done"))

(defun hyperb:autoloads-exist-p ()
  "Return t if all Hyperbole autoload files exist or nil otherwise."
  (and (file-readable-p (expand-file-name "hyperbole-autoloads.el" hyperb:dir))
       (file-readable-p (expand-file-name "kotl-autoloads.el"
					  (expand-file-name "kotl" hyperb:dir)))))

(defun hyperb:maybe-generate-autoloads ()
  "Ensure Hyperbole *-autoload.el files are already generated or generate them.
This is used only when running from git source and not a package release."
  (unless (hyperb:autoloads-exist-p)
    (hyperb:generate-autoloads)))

(defun hyperb:generate-autoloads ()
  "Renerate Hyperbole *-autoloads.el files whether they already exist or not."
  (let* ((default-directory hyperb:dir)
	 (backup-inhibited t)
	 (find-file-hook) ;; Prevent header insertion
	 (al-file (expand-file-name "hyperbole-autoloads.el")))
    ;; (make-local-variable 'generated-autoload-file)
    (with-current-buffer (find-file-noselect al-file)
      (make-directory-autoloads "." al-file))
    (setq al-file (expand-file-name "kotl/kotl-autoloads.el"))
    (with-current-buffer (find-file-noselect al-file)
      (make-directory-autoloads "." al-file)))
  (unless (hyperb:autoloads-exist-p)
    (error (format "Hyperbole failed to generate autoload files; try running 'make src' in a shell in %s" hyperb:dir))))

(defun hyperb:maybe-load-autoloads ()
  "Load Hyperbole autoload files that have not already been loaded."
  (let* ((default-directory hyperb:dir)
	 (hypb-autoloads (expand-file-name "hyperbole-autoloads.el"))
	 (kotl-autoloads (expand-file-name "kotl/kotl-autoloads.el")))
    (unless (featurep 'hyperbole-autoloads)
      (when (file-readable-p hypb-autoloads)
        (load-file hypb-autoloads)))
    (unless (featurep 'kotl-autoloads)
      (when (file-readable-p kotl-autoloads)
        (load-file kotl-autoloads)))))

;; This call loads the rest of the Hyperbole system.
(require 'hinit)

(defun hkey-set-key (key command)
  "Define a Hyperbole global minor mode KEY bound to COMMAND."
  (define-key hyperbole-mode-map key command))

(defun hkey-maybe-set-key (key command)
  "Define a Hyperbole global minor mode KEY bound to COMMAND only if not bound in current keymaps."
  (or (key-binding key)
      (where-is-internal command)
      (hkey-set-key key command)))

(defun hyperbole--enable-mode ()
  "Enable Hyperbole global minor mode."
  ;;
  ;; Abbreviate MSWindows /cygdrive mount point paths.
  (when (file-exists-p "/cygdrive")
    (add-to-list 'directory-abbrev-alist hyperb:cygdrive))
  ;; When running under a POSIX system with possible access to MSWindows servers,
  ;; cache valid MSWindows mount points.
  (hpath:cache-mswindows-mount-points)
  ;;
  ;; Save button attribute file whenever same dir file is saved and
  ;; `ebut:hattr-save' is non-nil.
  (add-hook (if (boundp 'write-file-functions)
		'write-file-functions
	      'write-file-hooks)
	    #'hattr:save t)
  ;;
  (hyperb:init-menubar))

(defun hyperbole--disable-mode ()
  "Disable Hyperbole keys, menus and hooks."
  ;; Deactivate hyperbole-mode
  ;; Delete Hyperbole menu from all menubars.
  (hui-menu-remove Hyperbole)
  ;;
  ;; Remove Hyperbole button comment from future outgoing mail.
  (when (boundp 'smail:comment) (setq smail:comment nil))
  (remove-hook 'after-init-hook #'hyperb:init)
  ;;
  (setq directory-abbrev-alist (remq hyperb:cygdrive
				     directory-abbrev-alist)
	hpath:posix-mount-point-to-mswindows-alist nil)
  ;;
  (remove-hook (if (boundp 'write-file-functions)
		   'write-file-functions
		 'write-file-hooks)
	       #'hattr:save))

;; This next expression initializes the Hyperbole keymap but does not
;; activate Hyperbole.  The only user-visible change it should make is
;; to globally bind {C-h h} to 'hyperbole' which when invoked will both
;; activate Hyperbole and show its minibuffer menu.
(if after-init-time
    ;; Initialize Hyperbole key bindings and hooks.
    (hyperb:init)
  ;; Initialize after other key bindings are loaded at startup.
  (add-hook 'after-init-hook #'hyperb:init t))

;; !! FIXME: This next expression activates Hyperbole for compatibility
;; with prior releases where (require 'hyperbole) was enough to
;; activate its key bindings.  However, loading a file should not
;; change Emacs's behavior, so after educating users to add this
;; next line to their Emacs initializations, it should be removed.
(hyperbole-mode 1)

(makunbound 'hyperbole-loading)

(provide 'hyperbole)

;;; hyperbole.el ends here
