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
))

;; disable backup file
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; key binding
(global-set-key (kbd "TAB") (kbd "C-q TAB"))
(global-set-key (kbd "C-x t n") 'tab-bar-new-tab)
(global-set-key (kbd "C-x t d") 'tab-bar-close-tab)

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

(electric-pair-mode)
(global-display-line-numbers-mode)
(tool-bar-mode -1)
(global-hl-line-mode)
(powerline-default-theme)

;; cursor
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))


;; treesitter
(global-tree-sitter-mode)


;; dashboard
(dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "Who even use Vi/Vim/NeoVim?")
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
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
