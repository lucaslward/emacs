;;; init.el --- Emacs entry point
;;
;; Copyright (c) 2013 Lucas Ward
;;
;; Author: Lucas Ward
;; URL: https://github.com/lucaslward/emacs
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; Code:

;; Turn off all of the weird ui stuff
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Let's leave the scroll bar around for now
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;Them stuff. For now just zenburn, but perhaps more later?
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "52b5da0a421b020e2d3429f1d4929089d18a56e8e43fe7470af2cea5a6c96443" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'zenburn t)

;; full screen on startup - I got this from emacs wiki. It said it was specific to OS X, so if
;; this file is used on Linux, it may need something different
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Add to load path
(add-to-list 'load-path "~/.emacs.d/core")

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Keybindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Setup package managers
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;;========================================
;; start the emacsserver that listens to emacsclient
(server-start)

;; Turn on 'interactive do'
(require 'ido)
    (ido-mode t)

;; Turn on yas snippets
(require 'yasnippet)
(yas-global-mode 1)

;; Turn on autopair globally
(require 'autopair)
(autopair-global-mode t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;=======================================
;; Add hooks

;; Enable flycheck for any buffer that can use it
(add-hook 'after-init-hook #'global-flycheck-mode)


;; duplicate current line
    (defun duplicate-current-line (&optional n)
      "duplicate current line, make more than 1 copy given a numeric argument"
      (interactive "p")
      (save-excursion
        (let ((nb (or n 1))
    	  (current-line (thing-at-point 'line)))
          ;; when on last line, insert a newline first
          (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
    	(insert "\n"))
    
          ;; now insert as many time as requested
          (while (> n 0)
    	(insert current-line)
    	(decf n)))))
    
    (global-set-key (kbd "s-d") 'duplicate-current-line)

;;; init.el ends here
