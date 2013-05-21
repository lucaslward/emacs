;; yari (with buttons) used to get ri docs
(require 'yari)
(require 'ruby-compilation)
(require 'rinari)
(require 'ruby-electric)
(require 'rvm)
(require 'rspec-mode)

(defun ri-bind-key ()
  "Bind f1 to yari."
  (local-set-key [f1] 'yari))

;; Ruby mode stuff
(add-hook 'ruby-mode-hook 'ri-bind-key)
(add-hook 'ruby-mode-hook (lambda()
			    (autopair-mode -1)
			    ))
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

(custom-set-variables '(rspec-use-rake-flag nil))
