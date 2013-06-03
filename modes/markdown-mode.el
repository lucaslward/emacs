(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)

(add-hook 'markdown-mode-hook (lambda() (auto-fill-mode t)))
