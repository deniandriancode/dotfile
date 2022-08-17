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
(global-set-key (kbd "C-x C-/") 'display-line-numbers-mode)

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

;; centaur tabs
(centaur-tabs-mode t)
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-bar 'left)
(setq centaur-tabs-set-modified-marker t)

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

;; cursor shape
(setq-default cursor-type 'bar)
