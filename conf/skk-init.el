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
  (setq skk-extra-jisyo-file-list
        (list "~/.emacs.d/skk-get-jisyo/SKK-JISYO.lisp"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.station"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.assoc"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.edict"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.law"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.jinmei"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.fullname"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.geo"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.itaiji"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.zipcode"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.okinawa"
              "~/.emacs.d/skk-get-jisyo/SKK-JISYO.propernoun"))
  ;; 全角・半角カタカナを変換候補にする
  (setq skk-search-katakana 'jisx0201-kana)
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
