(defun my:set-frame-width ()
  (interactive)
  (set-frame-width (selected-frame)
                   (my:set-frame--prompt "frame width" (frame-width))))
(defun my:set-frame-height ()
  (interactive)
  (set-frame-height (selected-frame)
                    (my:set-frame--prompt "frame height" (frame-height))))

(defun my:set-frame--prompt (prompt-message present-value)
  (if (window-system)
      (let ((width (string-to-number
                    (read-from-minibuffer
                     (format "%s(present: %s): " prompt-message present-value)))))
        (when (< width 1)
          (error (format "%s must be number is bigger than 0: %s" prompt-message width)))
        width)
    (error "Error: window system only")))
