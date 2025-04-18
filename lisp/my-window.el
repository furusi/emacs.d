;; -*- lexical-binding: t; -*-

;; 分割方法を切り替える(水平，垂直) (quoted from https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el)
(defun toggle-window-split ()
  "ウィンドウの分割方法を切り替える(水平→垂直，垂直→水平)"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(leaf toggle-window-split
  :bind ("C-x |" . toggle-window-split))

(provide 'my-window)
