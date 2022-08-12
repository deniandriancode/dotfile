;; adding melpa repository for installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; list of packages to be installed, add or remove the desired packages
(setq package-selected-packages
      '(doom-themes
	doom-modeline
	emmet-mode
	evil
	key-chord
	all-the-icons
	dart-mode
	haskell-mode
	nix-mode
	vertico
	auto-complete
	yasnippet
	yasnippet-snippets
	rjsx-mode
	treemacs
	treemacs-all-the-icons
	move-text
	multiple-cursors
	page-break-lines
	projectile
	dashboard
	use-package
	yaml-mode
	json-mode
	markdown-mode
	lua-mode))
