;;helm
(use-package helm
  :after migemo
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-s o" . helm-occur)
         ("C-x j" . helm-recentf)
         ("C-x r l" . helm-bookmarks))
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (helm-migemo-mode 1)
  (helm-autoresize-mode 1)
  (helm-mode 1))
(use-package helm-config
  :straight helm
  :after helm
  :config (helm-mode 1))
(use-package helm-swoop
  :ensure t)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package helm-rg
  :ensure t
  :ensure-system-package (rg . ripgrep))

