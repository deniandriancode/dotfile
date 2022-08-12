;; =============================
;; 
;; Author : Deni Andrian Prayoga
;; Email  : deniandriancode@gmail.com
;;
;; =============================
;;
;; Minimalist Emacs config with Evil mode, most of the configs are
;; inspired from Doom Emacs (https://github.com/doomemacs/doomemacs)

;; ===============================================
;; PACKAGES
;; ===============================================
;; load packages from packages.el
(load "~/.emacs.d/packages.el")
;; to install, add the package name and then run M-x package-install-selected-packages
;; DO NOT RUN M-x package-install <package-name>
;; which makes harder to track installed packages
;; ===========================
;; NOTE
;; ===========================
;; after running package-install-selected-packages, run M-x all-the-icons-install-fonts

;; load config file
(load "~/.emacs.d/config.el")

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
