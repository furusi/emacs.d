;;;  -*- lexical-binding: t -*-
(defun my-set-frame-width ()
  (interactive)
  (set-frame-width (selected-frame)
                   (my-set-frame--prompt "frame width" (frame-width))))
(defun my-set-frame-height ()
  (interactive)
  (set-frame-height (selected-frame)
                    (my-set-frame--prompt "frame height" (frame-height))))

(defun my-enlarge-frame ()
  (interactive)
  (set-frame-height (selected-frame)
                    (+ (frame-height) 2)))

(defun my-shrink-frame ()
  (interactive)
  (set-frame-height (selected-frame)
                    (+ (frame-height) -1)))

(defun my-enlarge-frame-horizontally ()
  (interactive)
  (set-frame-width (selected-frame)
                   (+ (frame-width) (* my-resize-frame-scale-width 2))))

(defun my-shrink-frame-horizontally ()
  (interactive)
  (set-frame-width (selected-frame)
                   (+ (frame-width) (* my-resize-frame-scale-width -1))))

(defun my-set-frame--prompt (prompt-message present-value)
  (if (window-system)
      (let ((width (string-to-number
                    (read-from-minibuffer
                     (format "%s(present: %s): " prompt-message present-value)))))
        (when (< width 1)
          (error (format "%s must be number is bigger than 0: %s" prompt-message width)))
        width)
    (error "Error: window system only")))

(defun factorial (n)
  (if (> n 0)
      (* n (factorial (- n 1)))
    1))

(defvar factorial-memo (make-hash-table :test 'equal))

(defun factorial-memo (n)
  "Return factorial of N"
  (cond ((= n 0) 1)
        ((= n 1) 1)
        ((gethash n factorial-memo)
         (gethash n factorial-memo))
        (t (let ((result (* n (factorial (- n 1)))))
             (puthash n result factorial-memo)
             result
             ))))

(defun combination (n r)
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

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

(leaf my-resize-frame
  :emacs>= 28
  :init
  (put 'my-enlarge-frame 'repeat-map 'resize-frame-repeat-map)
  (put 'my-shrink-frame 'repeat-map 'resize-frame-repeat-map)
  (put 'my-enlarge-frame-horizontally 'repeat-map 'resize-frame-repeat-map)
  (put 'my-shrink-frame-horizontally 'repeat-map 'resize-frame-repeat-map)
  :preface
  (defvar resize-frame-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "+")   #'my-enlarge-frame)
      (define-key map (kbd "=")   #'my-enlarge-frame)
      (define-key map (kbd "-")   #'my-shrink-frame)
      (define-key map (kbd "_")   #'my-shrink-frame)
      (define-key map (kbd ">")   #'my-enlarge-frame-horizontally)
      (define-key map (kbd "C-}") #'my-enlarge-frame-horizontally)
      (define-key map (kbd "<")   #'my-shrink-frame-horizontally)
      (define-key map (kbd "C-{") #'my-shrink-frame-horizontally)
      map
      ))
  :bind
  (("C-x C-{" . my-shrink-frame-horizontally)
   ("C-x C-}" . my-enlarge-frame-horizontally)))

(defun my-markdown--generate-link (text url &optional title)
  (format "[%s](%s)" text (if (eq title nil) url (format "%s \"%s\"" url title))))
(defun my-org--generate-link (text url)
  (format "[[%s][%s]]" url text))

(defun my-org--get-dedicated-targets-in-buffer ()
  "Return a list of dedicated targets in the current buffer."
  (save-excursion
    (let (targets)
      (goto-char (point-min))
      (while (re-search-forward "<<\\([^>]+\\\)>>" nil t)
        (let ((target (match-string 1)))
          (push target targets)))
      (nreverse targets))))

(defvar-local my-org-dedicated-targets-in-buffer nil
  "Lists of dedicated targets in a current buffer")


(provide 'my-lisp)
