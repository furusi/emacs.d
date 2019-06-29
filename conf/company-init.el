;;; package --- Summary"

;;; Code:

;;; Commentary:

(use-package company
  :ensure t
  :bind (
         ;; :map company-mode-map
         ;;      ((kbd "TAB") . 'company-indent-or-complete-common)
         :map company-active-map
              ("C-n"   . 'company-select-next)
              ("C-p"   . 'company-select-previous)
              ("C-s"   . 'company-filter-candidates)
              ("C-i"   . 'company-complete-selection)
         :map company-search-map
              ("C-n"   . 'company-select-next)
              ("C-p"   . 'company-select-previous))
  :config
  ;; (global-company-mode 1)
  (define-key company-mode-map (kbd "C-M-i") 'company-complete)
  (custom-set-variables
   '(company-idle-delay 0.1))
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
	(if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
		backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  ;; (push '(company-semantic :with company-yasnippet) company-backends)
  ;(custom-set-variables '(company-idle-delay nil))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-enable-recompletion t))

