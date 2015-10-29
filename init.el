;;(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config" "~/.emacs.d"))

;; We load them in the following order:
;; 1. Prelude: sets up settings/functions used by rest of the configuration
;; 2. Packages: install/configure (most) requisite packages
;; 3. Misc: various settings/default modes, goes after package install to avoid
;;          being overwritten.

(require 'config-prelude)
(require 'config-packages)
(require 'config-eshell)
(require 'config-misc)
(require 'config-theme)
