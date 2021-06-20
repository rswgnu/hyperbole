;; Use this in your Emacs init file to install Straight
(progn
  (defvar bootstrap-version)
  (setq package-enable-at-startup nil)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Then use this to install Hyperbole
(straight-use-package
 '(hyperbole
   :host nil
   :repo "https://git.savannah.gnu.org/git/hyperbole.git"
   :config (hyperbole-mode 1)))

(hyperbole-mode 1)

(message "%s" "Hyperbole successfully installed")
