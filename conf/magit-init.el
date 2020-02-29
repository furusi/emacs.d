;;; magit-init.el --- magit's configuration

;;; Commentary:

;;; Code:

;;git config --global user.name "John Doe"
;;git config --global user.email johndoe@example.com
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk 'all))

(provide 'magit-init)
;;; magit-init.el ends here
