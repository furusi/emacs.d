;;; magit-init.el --- magit's configuration

;;; Commentary:

;;; Code:

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  )
