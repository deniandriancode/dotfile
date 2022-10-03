(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(
	emmet-mode
	use-package
	doom-themes
	yasnippet
	yasnippet-snippets
	powerline-evil
	lua-mode
	rust-mode
	all-the-icons
	tree-sitter
	tree-sitter-langs
	dashboard
	which-key
	vertico
	company
	neotree
	markdown-mode
))

;; disable backup file
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; company mode
(global-company-mode 1)
;; (global-set-key (kbd "M-/") 'company-dabbrev)  ;; override dabbrev key-binding
(setq company-minimum-prefix-length 1)  ;; set minimal prefix
(setq company-idle-delay 0)

;; neotree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-fixed-size nil)
(global-set-key (kbd "C-c o f") 'neotree-toggle)

;; key binding
(global-set-key (kbd "TAB") (kbd "C-q TAB"))
;; (global-set-key (kbd "<backtab>") (kbd "TAB"))
(global-set-key (kbd "C-x t n") 'tab-bar-new-tab)
(global-set-key (kbd "C-x t d") 'tab-bar-close-tab)
(global-set-key (kbd "C-<return>") (kbd "C-e RET"))

;; half scrolling
(defun window-half-height ()
(max 1 (/ (1- (window-height (selected-window))) 2)))
   
(defun scroll-up-half ()
(interactive)
(next-line (window-half-height)))
   
(defun scroll-down-half ()         
(interactive)                    
(previous-line (window-half-height)))
   
(global-set-key [next] 'scroll-up-half)
(global-set-key [prior] 'scroll-down-half)

;; (electric-pair-mode)
(electric-indent-mode -1)
(add-hook 'prog-mode-hook '(lambda ()
			     (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(tool-bar-mode -1)
(global-hl-line-mode)
(powerline-default-theme)

;; cursor
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;; disable scroll bar
(scroll-bar-mode -1)

;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; vertico
(vertico-mode)

;; Different scroll margin
(setq vertico-scroll-margin 0)

;; Show more candidates
(setq vertico-count 20)

;; Grow and shrink the Vertico minibuffer
;; (setq vertico-resize t)

;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
(setq vertico-cycle t)

;; which key
(which-key-mode)

;; treesitter
(global-tree-sitter-mode)

;; terminal mode
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode -1)))

;; dashboard
(dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "Write some code")
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
;; (setq dashboard-show-shortcuts nil)

;; footer message
(setq dashboard-set-footer nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
