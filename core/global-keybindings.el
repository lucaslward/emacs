;;; global-keybindings.el --- useful keybindings.
;;
;; Author: Lucas Ward 
;; Version: 1.0.0
;; Keywords: convenience
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; All of the keybinding changes specific to my setup
;;
;;; Code:

;; All keybindings I use should be in here
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
;; Change kill-line to work for both lines and regions
(global-set-key (kbd "C-k") 'luca-kill-current-line-or-region)
;; Change font size should use the same keys as everything else on OSX
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-/") 'luca-comment-or-uncomment-current-line-or-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "s-d") 'luca-duplicate-current-line-or-region)
(global-set-key (kbd "C-\\") 'luca-indent-region-or-buffer)
(global-set-key (kbd "C-.") 'hippie-expand)
(global-set-key (kbd "<C-tab>") 'winner-undo)

;;; global-keybindings.el ends here
