;; .emacs

(when (< emacs-major-version 27)
  (error "Hyperbole requires Emacs 27 or above; you are running version %d" emacs-major-version))
(require 'package)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(unless (package-installed-p 'hyperbole)
  (package-refresh-contents)
  (package-install 'hyperbole))
(hyperbole-mode 1)

(message "%s" "Hyperbole successfully installed and activated")
