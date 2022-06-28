(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 1)
 '(ac-delay 0.01)
 '(centaur-tabs-mode t nil (centaur-tabs))
 '(centaur-tabs-set-icons t)
 '(centaur-tabs-show-navigation-buttons t)
 '(column-number-mode t)
 '(custom-enabled-themes '(doom-material-dark))
 '(custom-safe-themes
   '("2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "79586dc4eb374231af28bbc36ba0880ed8e270249b07f814b0e6555bdcb71fab" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" default))
 '(display-time-mode t)
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(neo-window-fixed-size nil)
 '(org-agenda-files '("/home/deni/Documents/AGENDA.org"))
 '(package-selected-packages
   '(centaur-tabs doom-themes spacemacs-theme afternoon-theme darcula-theme key-chord typescript-mode evil vue-mode lsp-haskell scss-mode vscode-dark-plus-theme material-theme web-mode neotree treemacs lsp-java company lsp-ui lsp-pyright use-package lsp-mode restclient all-the-icons projectile page-break-lines dashboard vimrc-mode haskell-mode lua-mode slime kotlin-mode auto-complete one-themes solarized-theme emmet-mode))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(treemacs-indent-guide-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrainsMono Nerd Font" :foundry "JB  " :slant normal :weight normal :height 98 :width normal))))
 '(cursor ((t (:background "#a89984" :foreground "#282828" :inverse-video t :width condensed))))
 '(tab-bar-tab-inactive ((t (:background "grey18" :inherit tab-bar-tab)))))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode 1))

(electric-pair-mode 1)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'auto-complete-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'kotlin-mode-hook 'auto-complete-mode)

(ac-config-default)
(global-auto-complete-mode 1)
(centaur-tabs-mode 1)

;; keybindings
(global-set-key (kbd "M-p") 'electric-buffer-list)
(global-set-key (kbd "C-.") 'compile)
(global-set-key (kbd "C-,") 'recompile)
(global-set-key (kbd "M-o") 'treemacs)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'dashboard)
(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner 'logo)
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
    (require 'lsp-pyright)
    (lsp))))  ; or lsp-deferred

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)

;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-video-player)
;; (require 'eaf-terminal)

;; apply syntax
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-hook 'web-mode-hook 'emmet-mode)

;; (require 'lsp)
;; (require 'lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

(require 'evil)
(evil-mode 1)
