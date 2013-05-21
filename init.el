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

;; Define global variables for the important path names. I prepend
;; everything with 'luca' to ensure I don't run into any conflicts

(defvar luca-core-dir (expand-file-name "core" user-emacs-directory)
  "The directory containing all core functaionlity, such as
  keybindings and mappings")
(defvar luca-lib-dir (expand-file-name "lib" user-emacs-directory)
  "The directory containing all third party 'libraries'")
(defvar luca-defuns-dir (expand-file-name "defuns" user-emacs-directory)
  "The directory containing all functions used in my emacs setup")
(defvar luca-modes-dir (expand-file-name "modes" user-emacs-directory)
  "The directory containing all functions used in my emacs setup")

;; define a global varible for whether or not emacs is running on a mac
(defvar is-mac (equal system-type 'darwin))

;; Turn off splash screen
(setq inhibit-startup-message t)

;; Turn off all of the weird ui stuff
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Let's leave the scroll bar around for now
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; full screen on startup - I got this from emacs wiki. It said it was
;; specific to OS X, so if this file is used on Linux, it may need
;; something different
(when is-mac (set-frame-parameter nil 'fullscreen 'fullboth))

;; Setup load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path luca-core-dir)
(add-to-list 'load-path luca-lib-dir)
(add-to-list 'load-path luca-defuns-dir)

;; Add the themes directory to the custom them load path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; Load zenburn theme by default
(load-theme 'zenburn t)

;; Everything from 'emacs customization' should be in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Setup environment variables from the user's shell.
(require 'exec-path-from-shell)
(when is-mac (exec-path-from-shell-initialize))

;; Setup package managers
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Functions (load all files in defuns-dir)
;; I'm not sure how I feel about this. I pulled it from the Emacs
;; Rocks guy. It feels like It should use more of the provide/require
;; mechanism
(dolist (file (directory-files luca-defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(dolist (file (directory-files luca-modes-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;;========================================
;; start the emacsserver that listens to emacsclient
(server-start)

;; Global 'require' of third party libs
(require 'ido)
(require 'yasnippet)
(require 'autopair)

;; My personal stuff
(require 'global-keybindings)
(require 'mode-mappings)

;; Interactive do
(ido-mode t)

;; yasnippet
(yas-global-mode 1)

;; autopair
(autopair-global-mode t)

;; Turn on highlight line
(global-hl-line-mode t)

;; Turn on line numbers for all files
(global-linum-mode t)

;; Turn on column numbers. No idea why it doesn't have a 'global variant
(column-number-mode t)

;; stop the cursor from blinking
(blink-cursor-mode 0)

;; selected text will be deleted if you type in it, rather than inserting before hand
(delete-selection-mode t)

;; show matching parens
(show-paren-mode t)

;; y and n should be sufficient
(fset 'yes-or-no-p 'y-or-n-p)

;;=======================================
;; Add hooks

;; Enable flycheck for any buffer that can use it
(add-hook 'after-init-hook #'global-flycheck-mode)


(ansi-color-for-comint-mode-on)

;; Elisp stuff
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

;; Turn off alarm bells
(setq ring-bell-function 'ignore)

;; load nxhtml
(load "~/.emacs.d/nxhtml/autostart.el")

;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

;;; init.el ends here
