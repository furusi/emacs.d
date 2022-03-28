;;;  -*- lexical-binding: t -*-
(defun my-set-frame-width ()
  (interactive)
  (set-frame-width (selected-frame)
                   (my-set-frame--prompt "frame width" (frame-width))))
(defun my-set-frame-height ()
  (interactive)
  (set-frame-height (selected-frame)
                    (my-set-frame--prompt "frame height" (frame-height))))

(defun my-set-frame--prompt (prompt-message present-value)
  (if (window-system)
      (let ((width (string-to-number
                    (read-from-minibuffer
                     (format "%s(present: %s): " prompt-message present-value)))))
        (when (< width 1)
          (error (format "%s must be number is bigger than 0: %s" prompt-message width)))
        width)
    (error "Error: window system only")))

(defun factoral (n)
  (if (> n 0)
      (* n (factoral (- n 1)))
    1))

(defun combination (n r)
  (/ (factoral n) (* (factoral r) (factoral (- n r)))))

(defvar my-resize-window-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (mapc (lambda (char)
            (define-key map (string char)
              'my-resize-window--help-message))
          (append (number-sequence #x21 #x70) (number-sequence #x72 #x7F)))
    (mapc (lambda (e)
            (define-key map (car e)
              (lambda ()
                (interactive)
                (when (cdr e)
                  (apply (cadr e) (cddr e))
                  (my-resize-window--help-message))
                (when (member (car e) '("\C-t" "\C-xo"))
                  (use-local-map my-resize-window--original-map)
                  (setq-local my-resize-window--original-map nil)
                  (other-window 1)
                  (message "quit.")))))
          '((">" . (enlarge-window-horizontally 1))
            ("<" . (shrink-window-horizontally 1))
            ("+" . (enlarge-window 1))
            ("=" . (enlarge-window 1))
            ("-" . (shrink-window 1))
            ("_" . (shrink-window 1))
            ("\C-t" . nil)
            ("\C-xo" . nil)))
    map
    ))

(defun my-resize-window--help-message ()
  (interactive)
  (message "+/-: change height, >/<: change width, q: quit"))

(defun my-resize-window-interactively ()
  ""
  (interactive)
  (when (and (eq (frame-width) (window-width))
             (eq (frame-height) (1+ (window-height))))
    (error "error: single window"))
  (let ((map (copy-keymap my-resize-window-map)))
    (setq-local my-resize-window--original-map (current-local-map))
    (my-resize-window--help-message)
    (define-key map (string ?q)
      (lambda ()
        (interactive)
        (use-local-map my-resize-window--original-map)
        (setq-local my-resize-window--original-map nil)
        (message "quit")))
    (use-local-map map)
    ))

(defvar my-resize-frame-scale-width 3)
(defvar my-resize-frame-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "+" (lambda () (interactive)
                          (set-frame-height (selected-frame)
                                            (+ (frame-height) 2))))
    (define-key map "=" (lambda () (interactive)
                          (set-frame-height (selected-frame)
                                            (+ (frame-height) 2))))
    (define-key map "-" (lambda () (interactive)
                          (set-frame-height (selected-frame)
                                            (+ (frame-height) -1))))
    (define-key map "_" (lambda () (interactive)
                          (set-frame-height (selected-frame)
                                            (+ (frame-height) -1))))
    (define-key map ">" (lambda () (interactive)
                          (set-frame-width (selected-frame)
                                           (+ (frame-width) (* my-resize-frame-scale-width 2)))))
    (define-key map "<" (lambda () (interactive)
                          (set-frame-width (selected-frame)
                                           (+ (frame-width) (* my-resize-frame-scale-width -1)))))
    map
    ))

;;;###autoload
(defun my-resize-frame-interactively ()
  ""
  (interactive)
  (unless window-system
    (error "This function works GUI mode only."))
  (let ((map (copy-keymap my-resize-frame-map)))
    (setq-local my-resize-frame--original-map (current-local-map))
    (my-resize-window--help-message)
    (define-key map (string ?q)
      (lambda ()
        (interactive)
        (use-local-map my-resize-frame--original-map)
        (setq-local my-resize-frame--original-map nil)
        (message "quit")))
    (use-local-map map)
    ))

(provide 'misc)
