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

(defun luca-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun luca-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
          (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
        (luca-indent-buffer)
        (message "Indented buffer.")))))
