;;; skk-init.el --- skkの設定

;;; Commentary:

;;; Code:

;;git config --global user.name "John Doe"
;;git config --global user.email johndoe@example.com

(use-package ddskk
  :straight (ddskk :type git :host github :repo "skk-dev/ddskk")
  :bind (("C-x C-j" . skk-mode))
  :init
  (setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
  (setq skk-search-katakana t)
  (setq skk-use-act t)
  (setq skk-henkan-show-candidates-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (setq-default skk-kutouten-type 'en)
  ;; 動的補完
  (setq skk-dcomp-activate t)
  (setq skk-rom-kana-rule-list
        '(("tni" nil ("ティ" . "てぃ"))
          ("dni" nil ("ディ" . "でぃ"))))
  (add-hook 'dired-load-hook
            (load "dired-x")
            (global-set-key "\C-x\C-j" 'skk-mode))
  (setq skk-egg-like-newline t);;non-nilにするとEnterでの確定時に改行しない
  ;; ▼モードで BS を押したときには確定しないで前候補を表示する
  (setq skk-delete-implies-kakutei nil)
  (require 'skk-study))

(provide 'skk-init)
;;; skk-init.el ends here
