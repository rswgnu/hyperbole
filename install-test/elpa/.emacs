;; .emacs

(require 'package)
(when (< emacs-major-version 27)
  (package-initialize))
(unless (package-installed-p 'hyperbole)
  (package-refresh-contents)
  (package-install 'hyperbole))
(hyperbole-mode 1)

(message "%s" "Hyperbole successfully installed")
