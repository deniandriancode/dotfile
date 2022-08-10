;; =============================
;; 
;; Author : Deni Andrian Prayoga
;;
;; =============================
;;
;; Minimalist Emacs config with Evil mode, most of the configs are
;; inspired from Doom Emacs (https://github.com/doomemacs/doomemacs)

;; ===============================================
;; PACKAGES
;; ===============================================
;; adding melpa repository for installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; list of packages to be installed
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
	json-mode))
;; to install, use M-x package-install-selected-packages
;; ===========================
;; NOTE
;; ===========================
;; after running package-install-selected-packages, run M-x all-the-icons-install-fonts

;; =====================
;; CONFIG
;; =====================
;; display line number
(when (version<="26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; custom key binding
(global-set-key (kbd "C-x C-'") 'auto-complete-mode)
(global-set-key (kbd "C-x C-<return>") 'treemacs)
(global-set-key (kbd "C-x C-\]") 'evil-mode)

;; doom mode line
(doom-modeline-mode 1)

;; adding directory to load-path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; emmet mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'rjsx-mode-hook  'emmet-mode)

;; =================================
;; NOTE
;; =================================
;; the following config must be in order
;; enable evil mode (Emacs Vi Layer) | +START_CONFIG
(evil-mode 1)
;; evil custom key binding
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-r") 'undo-redo)
;; | +END_CONFIG

;; use either move-text of move-lines, note that to move multiple lines
;; don't select using -- VISUAL LINE --, use -- VISUAL -- instead
;; move text default binding
(move-text-default-bindings)

;; move-lines package
;; (require 'move-lines)
;; (move-lines-binding)

;; treemacs customization
(treemacs-resize-icons 15)
(setq treemacs-width-is-initially-locked nil) 

;; hilghlight current line
(global-hl-line-mode)

;; dashboard config
(dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "I love Vim")
;; Set the banner
(setq dashboard-startup-banner 'logo)
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts t)
(setq dashboard-set-navigator t)
(setq dashboard-set-footer nil)

;; multiple cursors | FIXME
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; autopairs
(electric-pair-mode 1)

;; hide toolbar
(tool-bar-mode 0)

;; enable vertico
(vertico-mode)

;; enable yasnippet
(yas-global-mode)

;; map jk to <ESC> in evil mode
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; disable cursor blink
(setq blink-cursor-mode nil)

;; ==========================
;; EASY CUSTOMIZATION
;; ==========================
;; the following config generated from 'Easy Customization' in Emacs, you can delete it
;; and replace with your own customization
;; Setting Customized :
;; * theme : doom-one

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" default))
 '(global-display-line-numbers-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
