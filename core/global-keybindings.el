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
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "s-d") 'duplicate-current-line)
(global-set-key (kbd "C-\\") 'luca-indent-region-or-buffer)

(provide 'global-keybindings)

;;; global-keybindings.el ends here
