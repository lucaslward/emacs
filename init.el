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
(defvar luca-savefile-dir (expand-file-name "savefiles" user-emacs-directory)
  "The directory containing all functions used in my emacs setup")

(unless (file-exists-p luca-savefile-dir)
  (make-directory luca-savefile-dir))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" luca-savefile-dir))
(savehist-mode +1)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; define a global varible for whether or not emacs is running on a mac
(defvar is-mac (equal system-type 'darwin))

;; Turn off splash screen
(setq inhibit-startup-message t)

;; Turn off all of the weird ui stuff
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Let's leave the scroll bar around for now
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Setup load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path luca-core-dir)
(add-to-list 'load-path luca-lib-dir)
(add-to-list 'load-path luca-defuns-dir)

;; Add the themes directory to the custom them load path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; Load zenburn theme by default


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

(load-theme 'solarized-dark t)

(defun luca-load-files (directory)
  (dolist (file (directory-files directory t "\\w+"))
    (when (file-regular-p file)
      (load file))))

;;========================================
;; start the emacsserver that listens to emacsclient
(server-start)

;; Global 'require' of third party libs
(require 'ido)
(require 'yasnippet)
(require 'autopair)

;; Interactive do
(ido-mode t)

;; yasnippet
(yas-global-mode 1)

;; autopair
(autopair-global-mode t)

;; Turn on highlight line
(global-hl-line-mode t)
(set-face-attribute hl-line-face nil :underline nil)

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

(require 'drag-stuff)
(drag-stuff-global-mode t)

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

(defadvice switch-to-buffer (before save-buffer-now activate)
  "Invoke `prelude-auto-save-command' before `switch-to-window'."
  (luca-auto-save-command))
(defadvice other-window (before other-window-now activate)
  "Invoke `luca-auto-save-command' before `other-window'."
  (luca-auto-save-command))
(defadvice windmove-up (before other-window-now activate)
  "Invoke `luca-auto-save-command' before `windmove-up'."
  (luca-auto-save-command))
(defadvice windmove-down (before other-window-now activate)
  "Invoke `luca-auto-save-command' before `windmove-down'."
  (luca-auto-save-command))
(defadvice windmove-left (before other-window-now activate)
  "Invoke `luca-auto-save-command' before `windmove-left'."
  (luca-auto-save-command))
(defadvice windmove-right (before other-window-now activate)
  "Invoke `luca-auto-save-command' before `windmove-right'."
  (luca-auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'luca-auto-save-command)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(winner-mode t)

;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-undo-tree-mode t)

(require 'magit)

;; Clean stale buffers
(require 'midnight)

(setq-default fill-column 80)

;; Turn on the disabled stuff
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'volatile-highlights)
(volatile-highlights-mode t)

(require 'git-gutter-fringe)

;; Hide some minor modes. Because I didn't pass anything to it, it just hides it
;; completely. You can see what the mode line would look like with everything
;; turned on with M-x diminished modes
(require 'diminish)
(eval-after-load "git-gutter-fringe" '(diminish 'git-gutter-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "drag-stuff" '(diminish 'drag-stuff-mode))
(eval-after-load "volatile-highlights" '(diminish 'volatile-highlights-mode))
(eval-after-load "autopair" '(diminish 'autopair-mode))

(require 'wrap-region)
(wrap-region-global-mode t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(luca-load-files luca-core-dir)
(luca-load-files luca-defuns-dir)
(luca-load-files luca-modes-dir)

;; full screen on startup - I got this from emacs wiki. It said it was
;; specific to OS X, so if this file is used on Linux, it may need
;; something different
(when is-mac (set-frame-parameter nil 'fullscreen 'fullboth))

;;; init.el ends here
