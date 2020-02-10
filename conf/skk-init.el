;;; skk-init.el --- skkの設定

;;; Commentary:

;;; Code:

;;git config --global user.name "John Doe"
;;git config --global user.email johndoe@example.com

(use-package ddskk
  :ensure t
  :bind (("C-x C-j" . skk-mode))
  :init
  (setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
  (setq skk-search-katakana t)
  (setq skk-use-act t)
  (setq skk-henkan-show-candidates-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (setq-default skk-kutouten-type 'en)
  (setq skk-dcomp-activate t)
  (setq skk-rom-kana-rule-list
        '(("tni" nil ("ティ" . "てぃ"))
          ("dni" nil ("ディ" . "でぃ"))))
  ;; isearch
  ;; (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup) ; isearch で skk のセットアップ
  ;; (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup) ; isearch で skk のクリーンアップ
  ;; (setq skk-isearch-start-mode 'latin); isearch で skk の初期状態

  :config
  (setq skk-egg-like-newline t);;non-nilにするとEnterでの確定時に改行しない
  ;; ▼モードで BS を押したときには確定しないで前候補を表示する
  (setq skk-delete-implies-kakutei nil)
  (setq skk-dcomp-activate t))


(use-package skk-study
  :disabled nil
  :after ddskk)

;;wget https://raw.githubusercontent.com/skk-dev/ddskk/master/bayesian/skk-bayesian.el

(use-package skk-bayesian
  :disabled t
  :init
  (add-to-list 'load-path "~/.emacs.d/elisp/usr/"))

(provide 'skk-init)
;;; skk-init.el ends here
