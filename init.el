;;; init.el --- my init script

;;; Commentary:

;;; Code:


(show-paren-mode t)

;; 絵文字のフォント設定
(when window-system
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf custom-variables
  :doc "set custom variables"
  :custom
  ((backup-directory-alist . '((".*" . "~/.ehist")))
   (comment-style . 'multi-line)
   (default-frame-alist .'((width . 100)
                           (height . 40)))
   (dired-dwim-target . t)
   (ediff-window-setup-function . 'ediff-setup-windows-plain)
   (indent-tabs-mode . nil)
   (inhibit-startup-screen . t)
   (recentf-max-menu-items . 30)
   (recentf-max-saved-items . 2000)
   (recentf-auto-cleanup . 'never)
   (auto-save-interval . 10)
   (use-dialog-box . nil)
   (use-file-dialog . nil)
   (tool-bar-mode . nil)
   (menu-bar-mode . t)
   (safe-local-variable-values . '((org-export-directory . "~/Dropbox/org")))
   (vc-follow-symlinks . t))
  )

(leaf completion
  :emacs>= 27
  :config
  (push 'flex completion-styles)
  )



(leaf autorevert
  :custom
  ((auto-revert-interval . 0.1))
  :hook
  (emacs-startup-hook . global-auto-revert-mode)
  )

(leaf initchart
  :disabled t
  :straight (initchart :type git :host github :repo "yuttie/initchart")
  :require t
  :config
  (initchart-record-execution-time-of load file)
  (initchart-record-execution-time-of require feature))

(defun which-linux-distribution ()
  "from lsb_release"
  (interactive)
  (if (eq system-type 'gnu/linux)
      (shell-command-to-string "lsb_release -sd")
    ""))

(recentf-mode 1)

;;行番号を表示
(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      (setq-default indicate-empty-lines t)
      (setq-default indicate-buffer-boundaries 'left)))

(leaf exec-path-from-shell
  :straight t
  :require t
  :config
  (add-to-list 'exec-path-from-shell-variables "PYTHONPATH")
  (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package)

(use-package system-packages
  :config
  (when (eq system-type 'darwin)
    (setq system-packages-use-sudo nil
          system-packages-package-manager 'brew))
  (when (string= (car (split-string (which-linux-distribution))) "Ubuntu")
    (setq system-packages-use-sudo nil
          system-packages-package-manager 'brew))
  (when (string-match-p "arch" operating-system-release)
    (add-to-list 'system-packages-supported-package-managers
                 '(aurman .
                          ((default-sudo . nil)
                           (install . "aurman -S")
                           (search . "aurman -Ss")
                           (uninstall . "aurman -Rs")
                           (update . "aurman -Syu")
                           (clean-cache . "aurman -Sc")
                           (log . "cat /var/log/pacman.log")
                           (get-info . "aurman -Qi")
                           (get-info-remote . "aurman -Si")
                           (list-files-provided-by . "aurman -Ql")
                           (verify-all-packages . "aurman -Qkk")
                           (verify-all-dependencies . "aurman -Dk")
                           (remove-orphaned . "aurman -Rns $(pacman -Qtdq)")
                           (list-installed-packages . "aurman -Qe")
                           (list-installed-packages-all . "aurman -Q")
                           (list-dependencies-of . "aurman -Qi")
                           (noconfirm . "--noconfirm"))))
    (setq system-packages-use-sudo nil
          system-packages-package-manager 'aurman)))


(bind-keys ("C-c t l" . toggle-truncate-lines)
           ("C-t" . other-window)
           ("M-<f1>" . other-frame)  ;Macのショートカットに合わせる
           ("C-o" . my-insert-newline-and-indent)
           :map isearch-mode-map
           ("C-o" . isearch-exit))

;; C-u C-SPCの後C-SPCだけでマークを遡れる
(setq set-mark-command-repeat-pop t)
;; マークの数を32に増やす
(setq mark-ring-max 32)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)         ;文字列を折り返さない

;; テーマのディレクトリを設定
(setq custom-theme-directory (concat user-emacs-directory "themes/"))

(when (equal system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (when (or (eq window-system 'ns)
            (eq window-system 'mac))
    ;; 游教科書体
    ;; (set-face-attribute 'default nil
    ;;                     :family "YuKyokasho Yoko")
    ;; 源ノ角ゴシック
    (set-face-attribute 'default nil
                        :family "Noto Sans Mono CJK JP")
    (let* ((variable-tuple
            (cond ((x-list-fonts "Noto Sans Mono CJK JP") '(:font "Noto Sans Mono CJK JP"))
                  ((x-list-fonts "Source Sans Pro")       '(:font "Source Sans Pro"))
                  ((x-list-fonts "Lucida Grande")         '(:font "Lucida Grande"))
                  ((x-list-fonts "Verdana")               '(:font "Verdana"))
                  ((x-family-fonts "Sans Serif")          '(:family "Sans Serif"))
                  (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
           (headline           `(:inherit default :weight bold)))

      (custom-theme-set-faces
       'user
       `(org-level-8 ((t (,@headline ,@variable-tuple))))
       `(org-level-7 ((t (,@headline ,@variable-tuple))))
       `(org-level-6 ((t (,@headline ,@variable-tuple))))
       `(org-level-5 ((t (,@headline ,@variable-tuple))))
       `(org-level-4 ((t (,@headline ,@variable-tuple))))
       `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
       `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
       `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
       `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
    )
  )
(when (equal system-type 'gnu/linux)
  (add-to-list 'load-path "~/opt/mu-1.0/mu4e/")
  ;;曖昧な文字幅を指定する
  (aset char-width-table ?→ 2)

  (when (eq window-system 'x)
    (set-face-attribute 'default nil
                            :family "Noto Sans Mono CJK JP")))

;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
(setq use-default-font-for-symbols nil)


(leaf restart-emacs :straight t)

(leaf dired*
  :config
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil)))

(leaf xwidget-webkit
  :hook
  ((xwidget-webkit-mode-hook . (lambda ()
                                (display-line-numbers-mode -1)))))

(leaf sudo-edit :straight t)
(leaf projectile
  :bind ((:projectile-mode-map
          ("C-c p" . projectile-command-map)))
  :straight t
  :custom
  ((projectile-indexing-method . 'hybrid)
   (projectile-sort-order . 'recently-active)
   (projectile-switch-project-action . 'projectile-dired))
  :config
  (projectile-mode +1)
  (dolist
      (d '(".ccls-cache"))
    (add-to-list 'projectile-globally-ignored-directories d))
  )


;; ddskk
(leaf ddskk
  :straight (ddskk :type git :host github :repo "skk-dev/ddskk")
  :commands skk-mode
  :bind (("C-x C-j" . skk-mode)
         (:minibuffer-local-map
          ("C-j" . skk-kakutei)))
  :hook ((skk-load-hook . (lambda () (require 'context-skk))) ;自動的に英字モードになる
         ;; isearch
         (isearch-mode-hook . skk-isearch-mode-setup) ; isearch で skk のセットアップ
         (isearch-mode-end-hook . skk-isearch-mode-cleanup) ; isearch で skk のクリーンアップ
         (helm-exit-minibuffer-hook . skk-isearch-mode-cleanup))
  :custom
  ((skk-japanese-message-and-error . t)
   (skk-share-private-jisyo . t)
   (skk-isearch-start-mode . 'latin); isearch で skk の初期状態
   )
  :init
  (push (lambda ()
          (if (eq (current-column) 0)
              (org-at-heading-p)
            nil))
        context-skk-context-check-hook)
  (push (lambda ()
          (if (eq (current-column) 0)
              (org-at-block-p)
            nil))
        context-skk-context-check-hook)
  (setq skk-get-jisyo-directory (format "%sskk-get-jisyo/" user-emacs-directory))
  (setq skk-large-jisyo (format "%sSKK-JISYO.L" skk-get-jisyo-directory))
  (setq skk-extra-jisyo-file-list
        (mapcar (lambda (x)
                  (format "%s%s" skk-get-jisyo-directory x))
                '("SKK-JISYO.lisp" "SKK-JISYO.station"
                  "SKK-JISYO.assoc" "SKK-JISYO.edict"
                  "SKK-JISYO.law" "SKK-JISYO.jinmei"
                  "SKK-JISYO.fullname" "SKK-JISYO.geo"
                  "SKK-JISYO.itaiji" "SKK-JISYO.zipcode"
                  "SKK-JISYO.okinawa" "SKK-JISYO.propernoun")))
  ;; サ行変格活用の動詞も送りあり変換出来るようにする
  (setq skk-search-sagyo-henkaku t)
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
  (setq skk-egg-like-newline t);;non-nilにするとEnterでの確定時に改行しない
  ;; ▼モードで BS を押したときには確定しないで前候補を表示する
  (setq skk-delete-implies-kakutei nil)
  ;; @@ skk-search-web.el
  ;; (setq skk-use-search-web t)
  ;; (when skk-use-search-web
  ;; ;; 辞書変換が尽きたら Google CGI API for Japanese Input による変換を実行
  ;; ;; https://www.google.co.jp/ime/cgiapi.html
  ;; (add-to-list 'skk-search-prog-list
  ;; 	       '(skk-search-web 'skk-google-cgi-api-for-japanese-input)
  ;; 	       t))
  (setq skk-auto-insert-paren t)
  (add-hook  'dired-load-hook
             (load "dired-x")
             (global-set-key "\C-x\C-j" 'skk-mode))
  (leaf skk-study
    :require t)
  (leaf skk-hint
    :require t
    :config
    ;; ▼モード中で=漢字の読み方を指定する
    (setq skk-hint-start-char ?=))
  (leaf context-skk
    :config
    (add-to-list 'context-skk-programming-mode 'python-mode)
    (add-to-list 'context-skk-programming-mode 'rustic-mode)
    (add-to-list 'context-skk-programming-mode 'js-mode)
    (setq context-skk-mode-off-message "[context-skk] 日本語入力 off")
    (context-skk-mode))
  (defun skk-set-display-table ()
    (walk-windows (lambda (w)
                    (let ((disptab (make-display-table)))
                      (aset disptab ?\▼ (vector (make-glyph-code ?# 'escape-glyph)))
                      (aset disptab ?\▽ (vector (make-glyph-code ?@ 'escape-glyph)))
                      (set-window-display-table w disptab)))))
  (add-hook 'window-configuration-change-hook #'skk-set-display-table)
  (add-hook 'after-init-hook #'skk-set-display-table))


(leaf eww
  :commands (eww)
  :config
  (setq eww-search-prefix "https://www.google.co.jp/search?q=")
  (defun eww-disable-images ()
    "eww で画像表示させない"
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image-alt)
    (eww-reload))
  (defun eww-enable-images ()
    "eww で画像表示させる"
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image)
    (eww-reload))
  (defun shr-put-image-alt (spec alt &optional flags)
    (insert alt))
  ;; はじめから非表示
  (defun eww-mode-hook--disable-image ()
    (setq-local shr-put-image-function 'shr-put-image-alt))
  (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image))

(leaf magit
  :bind (("C-x g" . magit-status))
  :require t
  :straight t
  :custom
  ((magit-display-buffer-function . 'magit-display-buffer-fullframe-status-v1)
   (magit-diff-refine-hunk . 'all))
  :config
  ;; ediff時にorgファイルを全て表示する
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all)))
(leaf magit-svn
  :straight t)
(leaf grip-mode
  :straight t
  :bind ((:markdown-mode-command-map
          ("g" . grip-mode))))

(leaf migemo
  :straight t
  :require t
  :config
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-coding-system 'utf-8-unix)
  ;; Set your installed path
  (setq migemo-command
        (cond ((eq system-type 'darwin)    "cmigemo")
              ((eq system-type 'windows-nt)    "cmigemo")
              ((eq system-type 'gnu/linux) "/usr/bin/cmigemo")))
  (setq migemo-dictionary
        (cond ((eq system-type 'darwin)
               "/opt/homebrew/opt/cmigemo/share/migemo/utf-8/migemo-dict")
              ((eq system-type 'windows-nt)
               "~/opt/cmigemo-default-win64/dict/utf-8")
              ((string-match-p "arch" operating-system-release)
               "/usr/share/migemo/utf-8/migemo-dict")
              (t "/usr/share/cmigemo/utf-8/migemo-dict")))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init))



(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))


;; SLIMEのロード


(leaf undo-tree
  :straight t
  :diminish (global-undo-tree-mode undo-tree-mode)
  :require t
  :global-minor-mode global-undo-tree-mode)


(leaf auto-save-buffers-enhanced
  :disabled t
  :straight t
  :config
  ;; 1秒後に保存
  (setq auto-save-buffers-enhanced-interval 5)
  (auto-save-buffers-enhanced t)
  ;; Wroteのメッセージを抑制
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  ;; tramp mode時の自動保存を抑制
  (setq auto-save-buffers-enhanced-exclude-regexps '("^/rsync:" "^/ssh:" "^/scp:" "/sudo:" "/multi:" ".*.gpg$")))

(leaf real-auto-save
  :disabled t
  :straight t
  :require t
  :custom ((real-auto-save-interval . 0.5))
  :hook ((org-mode-hook prog-mode-hook) . real-auto-save-mode))


(leaf spaceline
  :straight t
  :require spaceline-config
  :diminish (auto-revert-mode abbrev-mode)
  :config
  (spaceline-emacs-theme)
  (spaceline-helm-mode))


;; helm
(leaf *helm
  :disabled t
  :config
  (leaf helm
    :straight t
    :require helm-config
    :diminish helm-migemo-mode
    :custom
    ((helm-candidate-number-limit . 300))
    :bind (("M-x"      . helm-M-x)
           ("M-y"      . helm-show-kill-ring)
           ("C-x b"    . helm-mini)
           ("C-x C-f"  . helm-find-files)
           ("M-s o"    . helm-occur)
           ("C-x j"    . helm-recentf)
           ("C-x r l"  . helm-bookmarks)
           (:helm-map
            ("<tab>"  . helm-execute-persistent-action) ;rebind tab to do persistent action
            ("C-i"    . helm-execute-persistent-action) ;make TAB works in terminal
            ("C-z"    . helm-select-action)             ;list actions using C-z
            )
           (:isearch-mode-map
            :package isearch
            ("C-i" . helm-occur-from-isearch)))
    :config
    (helm-mode 1)
    (helm-autoresize-mode 1)
    (helm-migemo-mode 1))
  (leaf helm-projectile
    :straight t
    :require t
    :custom
    (projectile-switch-project-action . #'projectile-dired)
    :config
    (helm-projectile-on))
  (leaf helm-swoop
    :straight t
    :disabled t)
  (leaf helm-lsp
    :straight t
    :commands helm-lsp-workspace-symbol)
  (leaf helm-rg
    :straight t
    ;;  :ensure-system-package (rg . ripgrep)
    )
  (leaf helm-make
    :straight t))

(leaf *ivy
  ;; :disabled t
  :config
  (leaf counsel
    :straight t
    :require ivy
    :diminish counsel-mode
    :custom
    ((counsel-switch-buffer-preview-virtual-buffers . nil)
     (ivy-count-format . "(%d/%d) ")
     (ivy-initial-inputs-alist . nil)
     (ivy-re-builders-alist . '((t . ivy--regex-ignore-order)))
     (ivy-use-virtual-buffers . t))
    :bind (("M-x" . counsel-M-x)
           ("C-x C-b" . counsel-ibuffer)
           ("C-x b" . counsel-switch-buffer)
           ("C-x C-f" . counsel-find-file)
           ("M-y" . counsel-yank-pop)
           ("C-x c i" . counsel-imenu)
           ("C-x j" . counsel-recentf)
           ("C-z" . ivy-dispatching-done)
           ("C-c i r" . ivy-resume)
           (:isearch-mode-map
            ("C-i" . swiper-from-isearch))
           (:ivy-minibuffer-map
            ("C-l" . counsel-up-directory)))
    :config
    (ivy-mode t)
    (counsel-mode t))
  (leaf all-the-icons-ivy-rich
    :straight t
    :require t
    :config
    (all-the-icons-ivy-rich-mode t))
  (leaf ivy-rich
    :straight t
    :require t
    :custom
    ((ivy-rich-path-style . 'absolute))
    :config
    (ivy-rich-mode t)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
  (leaf counsel-projectile
    :straight t
    :config
    (counsel-projectile-mode)
    (setcar counsel-projectile-switch-project-action 4))
  (leaf lsp-ivy
    :after lsp
    :straight t)
  (leaf ivy-migemo
    :straight t
    :bind
    ((:ivy-minibuffer-map
      ("M-f" . ivy-migemo-toggle-fuzzy)
      ("M-m" . ivy-migemo-toggle-migemo)))))

(leaf rg
  :bind (("C-c s" . rg-menu))
  :straight t
  :require t)
(leaf highlight-symbol
  :straight t
  :require t)
(leaf expand-region
  :straight t
  :require t
  :bind (("C-," . er/expand-region)))


(leaf all-the-icons :straight t)

(leaf which-key :straight t
  :diminish t
  :custom
  (which-key-idle-secondary-delay . 0.0)
  (which-key-max-description-length . 35)
  :config
  ;; 3つの表示方法どれか1つ選ぶ
  (which-key-setup-side-window-bottom)    ;ミニバッファ
  ;; (which-key-setup-side-window-right)     ;右端
  ;; (which-key-setup-side-window-right-bottom) ;両方使う
  (which-key-mode 1))

;;;yasnippet
(leaf yasnippet
  :straight t
  :require t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))
(leaf yasnippet-snippets :straight t)


(leaf keyfreq :straight t
  :config
  (setq keyfreq-excluded-commands
        '(
          backward-char
          dired-next-line
          forward-char
          helm-next-line
          helm-previous-line
          ignore
          keyboard-quit
          lsp-ui-doc--handle-mouse-movement
          magit-next-line
          magit-previous-line
          move-end-of-line
          next-line
          org-self-insert-command
          previous-line
          scroll-up-command
          self-insert-command
          skk-delete-backward-char
          skk-insert
          skk-previous-candidate
          ))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Emacs起動時にrst.elを読み込み
(leaf rst
  :straight t
  :bind ((:rst-mode-map
              ("M-RET" . rst-insert-list)))
  :config
  (when (eq system-type 'darwin)
    (setq rst-pdf-program "open -a Skim")
    (setq rst-slides-program "open -a Firefox")))

(leaf gradle-mode
  :require t
  :straight t
  :mode (("\\.gradle$" . gradle-mode)))


(leaf slime
  :straight slime-company
  :if (file-exists-p "~/.roswell/helper.el")
  :custom
  ((slime-auto-start . 'ask)
   (slime-company-completion . 'fuzzy)
   (slime-complete-symbol*-fancy . t))
  :hook ((lisp-mode-hook . slime-mode)
         (slime-repl-mode-hook
          . (lambda () (add-to-list
                        'company-backends
                        '(company-slime company-dabbrev-code)))))
  :init
  (load (expand-file-name "~/.roswell/helper.el"))
  :config
  ;; (slime-setup '(slime-fancy slime-company))
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-fancy slime-company slime-indentation))
  (defun slime-space\\skk-insert (origfun &rest arglist)
    "skkの変換(スペース)がslime-spaceに食われてしまうのを回避"
    (apply (cond (skk-henkan-mode
                  ;; skk-henkan-mode中であれば(▽▼の時)skk-insertへ処理を投げる
                  #'skk-insert)
                 (t
                  ;; それ以外は通常のslime-space
                  origfun))
           arglist))
  ;; (advice-add 'slime-space :around #'slime-space\\skk-insert)
  (advice-add 'slime-autodoc-space :around #'slime-space\\skk-insert)
  ;; (let ((path "/usr/local/share/doc/hyperspec/HyperSpec/"))
  ;;   (when (file-exists-p path)
  ;;   (setq common-lisp-hyperspec-root  path)
  ;;   (setq common-lisp-hyperspec-symbol-table
  ;;         (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")
  ;;         common-lisp-hyperspec-issuex-table
  ;;         (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))))
  )

(leaf web-mode
  :straight t
  :require t
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[gj]sp\\'"    . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.html?\\'"     . web-mode))
  :config
  (setq web-mode-extra-snippets
        '(("php" . (("print" . "print(\"|\")"))))))

;; Org-mode
(leaf org*
  :config
  (leaf org
    :mode (("\\.org$" . org-mode))
    :straight org-plus-contrib
    :custom
    ((org-export-allow-bind-keywords . t)
     (org-export-backends . '(ascii html icalendar latex md odt taskjuggler asciidoc))
     (org-id-link-to-org-use-id . 'create-if-interactive-and-no-custom-id)
     (org-icalendar-use-scheduled . '(event-if-todo todo-start))
     (org-link-file-path-type . 'relative)
     (org-list-allow-alphabetical . t)
     (org-return-follows-link . t)
     (org-agenda-start-on-weekday . 0)
     (org-link-frame-setup .
                           '((vm . vm-visit-folder-other-frame)
                             (vm-imap . vm-visit-imap-folder-other-frame)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl-other-frame)))
     (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
     (org-src-lang-modes . '(("arduino" . arduino)
                             ("browser" . html)
                             ("redis" . redis)
                             ("html" . web)
                             ("php" . php)
                             ("browser" . web)
                             ("ocaml" . tuareg)
                             ("elisp" . emacs-lisp)
                             ("ditaa" . artist)
                             ("asymptote" . asy)
                             ("sqlite" . sql)
                             ("calc" . fundamental)
                             ("C" . c)
                             ("cpp" . c++)
                             ("C++" . c++)
                             ("screen" . shell-script)
                             ("shell" . sh)
                             ("bash" . sh)
                             ("dot" . graphviz-dot)
                             ("asm" . asm)
                             ("python" . python)))
     (org-modules . '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
     (org-src-preserve-indentation . t)
     (org-startup-folded . t)
)
    :bind (("C-c c" . org-capture)
           ("C-c l" . org-store-link)
           ("C-c a" . org-agenda)
           ("<f2>" . insert-zero-width-space)
           (:org-mode-map
            ("C-c C-\'" . org-insert-structure-template)))
    :init
    (defun insert-zero-width-space()
      (interactive)
      (insert-char #x200b))
    (defun insert-zero-width-space-twice()
      (interactive)
      (insert-zero-width-space)
      (insert-zero-width-space))
    (setq org-directory
          (expand-file-name
           (if (file-exists-p "~/git/notes/")
               "~/git/notes/"
             (progn
               (when(not (file-exists-p "~/org/"))
                 (mkdir "~/org/"))
                 "~/org/"))))
    
    :custom
    ((org-preview-latex-default-process . 'dvisvgm)
     (org-startup-folded . t))
    :config
    
    ;; 強調の規則を変更(別の環境で開いた場合は認識されなくなる...)
    (setcar org-emphasis-regexp-components "-[:space:]\x200B('\"{")
    (setcar (nthcdr 1 org-emphasis-regexp-components) "-[:space:]\x200B.,:!?;'\")}\\[")
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
    
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 2.0))
    ;; org-modeの固定幅フォントを設定
    (let ((fontset (cond
                    ((eq window-system 'ns) "Noto Sans Mono CJK JP")
                    ((eq window-system 'x) "Noto Sans Mono CJK JP"))))
      (dolist (face '(org-table
                      org-formula
                      org-date))
        (set-face-attribute face nil :family fontset)))

    (add-to-list 'face-font-rescale-alist
                 '(".*IPAゴシック.*" . 0.85))

    (when (equal system-type 'darwin)
      (setq org-plantuml-jar-path
            "/usr/local/opt/plantuml/libexec/plantuml.jar"))


    (setq org-agenda-files
          (list
           (concat org-directory "agenda/")
           (concat org-directory "task.org")
           (concat org-directory "habit.org")
           (concat org-directory "event.org")
           (concat org-directory "inbox.org")
           (concat org-directory "productivity.org")
           (concat org-directory "org-ical.org")
           (concat org-directory "notes/")
           (concat org-directory "calendar/")))
    
    (setq org-refile-targets
          '((org-agenda-files :maxlevel . 2)))
    (setq org-tag-alist
          '(("ignore" . ?i) ("@OFFICE" . ?o) ("@HOME" . ?h) ("SHOPPING" . ?s)
            ("MAIL" . ?m) ("PROJECT" . ?p) ("備忘録" . ?b)))
    (setq org-capture-templates
          `(("i" "インボックス" entry
             (file ,(concat org-directory "inbox.org"))
             "* %? %i\n %U\n")
            ;; ("h" "定期的にやること" entry
            ;;  (file ,(concat org-directory "habit.org"))
            ;;  "* %?\n %U\n")
            ("t" "タスク" entry
             (file ,(concat org-directory "task.org"))
             "* TODO %? %i\n %U\n")
            ("e" "イベント" entry
             (file ,(concat org-directory "event.org"))
             "* EVENT %?\n %a\n %U\n")
            ("n"
             "ノート(本文から書く)"
             entry
             (file+headline, (concat org-directory "notes.org") "MEMO")
             "* %U \n%?")
            ("N"
             "ノート(見出しから書く)"
             entry
             (file+headline, (concat org-directory "notes.org") "MEMO")
             "* %U %?\n\n\n")
            ("r" "読みかけ(リンク付き)" entry
             (file ,(concat org-directory "reading.org"))
             "* %?\n %a\n %U\n")
            ("m"
             "みんなで会議"
             entry
             (file+olp+datetree (concat org-directory "minutes.org") "会議")
             "* %T %?"
             :empty-lines 1
             :jump-to-captured 1)
            ("p"
             "ぱっと 読み返したいと思ったとき"
             plain
             (file+headline nil "PLAIN")
             "%?"
             :empty-lines 1
             :jump-to-captured 1
             :unnarrowed 1)
            ("g"
             "とりあえず 仕事を放り込む"
             entry
             (file+headline (concat org-directory "gtd.org") "GTD")
             "** TODO %T %?\n   Entered on %U    %i\n"
             :empty-lines 1)
            ("i"
             "itemのテスト"
             item
             (file+headline (concat org-directory "gtd.org") "GTD")
             "** TODO %T %?\n   Entered on %U    %i\n"
             :empty-lines 1)
            ("z"
             "'あれ'についてのメモ"
             entry
             (file+headline , (concat org-directory "notes.org") "MEMO")
             "* %U %? %^g\n\n"
             :empty-lines 1)))
    ;;
    (setq org-agenda-default-appointment-duration 60)
    ;; コードを評価するとき尋ねない
    (setq org-confirm-babel-evaluate nil)

    (add-to-list 'org-babel-tangle-lang-exts
                 '("C" . "c"))
    ;; 有効にする言語 デフォルトでは elisp のみ
    (org-babel-do-load-languages
     'org-babel-load-languages '((C          . t)
                                 (org        . t)
                                 (python     . t)
                                 (lisp       . t)
                                 (emacs-lisp . t)
                                 (ruby       . t)
                                 (plantuml   . t)
                                 (java       . t)
                                 (gnuplot    . t)
                                 (perl       . t)
                                 (dot        . t)))

    (setq org-use-speed-commands t)
    (setq org-icalendar-alarm-time 30)
    (setq org-icalendar-timezone "Asia/Tokyo")

    ;; htmlで数式
    (setf org-html-mathjax-options
          '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
            (scale "100")
            (align "center")
            (indent "2em")
            (mathml nil)))
    (setf org-html-mathjax-template
          "<script type=\"text/javascript\" src=\"%PATH\"></script>")
    (defun org-todo-list-current-file (&optional arg)
      "Like `org-todo-list', but using only the current buffer's file."
      (interactive "P")
      (let ((org-agenda-files (list (buffer-file-name (current-buffer)))))
        (if (null (car org-agenda-files))
            (error "%s is not visiting a file" (buffer-name (current-buffer)))
          (org-todo-list arg))))

    (defun my-org-mode-hook ()
      (add-hook 'completion-at-point-functions
                'pcomplete-completions-at-point nil t)
      ;; (face-remap-add-relative 'default :height 173)
      )
    (org-babel-do-load-languages
     'org-babel-load-languages org-babel-load-languages)
    (add-hook 'org-mode-hook #'my-org-mode-hook)
    ;;ob-plantuml
    (add-to-list 'org-babel-default-header-args:plantuml
                 '(:cmdline . "-charset utf-8"))
    (setq org-publish-project-alist
          '(("aip3"
             :base-directory "~/git/advancedinformationprocessing3/org"
             :publishing-directory "~/git/advancedinformationprocessing3/pub"
             :base-extension "org"
             :publishing-function org-html-publish-to-html
             :html-postamble "<a href=\"index.html\">サイトのトップへ戻る</a>"
             :language "ja"
             :with-tags nil
             ;; :auto-sitemap t
             :htmlized-source t
             :with-tags nil
             :makeindex t
             :recursive t)
            ("aip3-image"
             :base-directory "~/git/advancedinformationprocessing3/image"
             :publishing-directory "~/git/advancedinformationprocessing3/pub/image"
             :base-extension "jpg\\|png\\|pdf"
             :publishing-function org-publish-attachment
             :recursive t))))
  (leaf org-protocol
    :require t
    :config
    (add-to-list 'org-modules 'org-protocol))
  (leaf org-mu4e
    :disabled t
    :straight t
    :after (org mu4e)
    :config
    ;;store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil))
  (leaf ox-rst
    :straight t
    :after (org)
    :custom
    ((org-rst-headline-underline-characters . '(45 126 94 58 39 32 95))))
  (leaf ox-hugo
    :straight t
    :after org)
  (leaf ob-browser
    :straight t
    :after org)
  (leaf ob-java
    :custom
    ((org-babel-java-compiler . "javac -encoding UTF-8")))
  (leaf ox-epub
    :straight t
    :after org)
  (leaf ox*
    :after org
    :custom
    (org-export-allow-bind-keywords . t)
    :config
    (defvar org-export-directory nil
      "org-exportの出力先を指定する変数。buffer-local変数として指定する。")
    (defun org-export-output-file-name--set-directory (orig-fn extension &optional subtreep pub-dir)
      (setq pub-dir (or pub-dir org-export-directory))
      (funcall orig-fn extension subtreep pub-dir))
    (advice-add 'org-export-output-file-name :around 'org-export-output-file-name--set-directory)
    (leaf ox-pandoc
      :after org
      :straight t
      :require t
      :if (or (file-exists-p "/usr/local/bin/pandoc")
              (file-exists-p "/opt/local/bin/pandoc")
              (file-exists-p "/opt/homebrew/bin/pandoc"))))
  
  (leaf org-roam
    :after org
    :straight t
    :hook
    ((org-mode-hook . org-roam-mode))
    :custom
    `((org-roam-directory . ,(concat org-directory "roam/")))
    :bind
    ((:org-roam-mode-map
      ("C-c n l" . org-roam)
      ("C-c n f" . org-roam-find-file)
      ("C-c n g" . org-roam-graph))
     (:org-mode-map
      ("C-c n i" . org-roam-insert)
      ("C-c n I" . org-roam-insert-immediate)))
    :config
    (when (eq system-type 'darwin)
          (setq org-roam-graph-viewer "open"))
    (leaf org-roam-protocol
      :require t
      )
    (leaf org-roam-server
      :straight t
      :require t)
    )
  (leaf org-brain
    :straight t
    :after org
    :require t
    :bind
    ((:org-mode-map
      ("C-c b" . org-brain-prefix-map)))
    )
  )


(leaf org-journal
  :straight t
  :require t
  :after org
  :commands org-journal-new-entry
  :custom
  `((org-journal-file-type . 'monthly)
    (org-journal-dir . ,(concat org-directory "roam/journal/"))
    (org-journal-enable-agenda-integration . t)
    (org-journal-date-format . "%F (%a)")
    (org-journal-time-format . "<%Y-%m-%d %R> ")
    (org-journal-file-format . "%Y%m.org")
    (org-journal-file-header . "# -*- mode: org-journal; -*-")))



;; Org Mode LaTeX Export

(leaf org-eldoc
  :after org
  :require t
  :hook (org-mode-hook . eldoc-mode)
  :config
  (defadvice org-eldoc-documentation-function (around add-field-info activate)
    (or
     (ignore-errors (and (not (org-at-table-hline-p))
                         (org-table-field-info nil)))
     ad-do-it))
  (eldoc-add-command-completions
   "org-table-next-" "org-table-previous" "org-cycle"))


(leaf ox-latex
  :require t
  :straight nil
  :after (org)
  :custom ((org-latex-minted-options . '(("frame" "single")
                                         ("breaklines" "")
                                         ("style" "xcode")
                                         ("fontsize" "\\footnotesize")))
           (org-latex-compiler . "lualatex")
           (org-latex-default-class . "lualatex-jlreq")
           (org-latex-listings . 'minted)
           (org-latex-listings-options . '(("frame" "single")
                                           ("basicstyle" "{\\ttfamily\\scriptsize}")
                                           ("numbers" "left")
                                           ("commentstyle" "{\\ttfamily\\scriptsize}")
                                           ("breaklines" "true")
                                           ("showstringspaces" "false")))
           (org-latex-minted-langs . '((rust "rust")
                                       (emacs-lisp "common-lisp")
                                       (cc "c++")
                                       (cperl "perl")
                                       (shell-script "bash")
                                       (caml "ocaml")
                                       (bash "bash")
                                       (conf "ini")))
           (org-preview-latex-default-process . 'dvisvgm)
           (org-preview-latex-process-alist . '((dvipng :programs
                                                        ("latex" "dvipng")
                                                        :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
                                                        (1.0 . 1.0)
                                                        :latex-compiler
                                                        ("latex -interaction nonstopmode -output-directory %o %f")
                                                        :image-converter
                                                        ("dvipng -D %D -T tight -o %O %f"))
                                                (dvisvgm :programs
                                                         ("latex" "dvisvgm")
                                                         :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
                                                         (1.7 . 1.5)
                                                         :latex-compiler
                                                         ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                                                         :image-converter
                                                         ("dvisvgm %f -n -b min -c %S -o %O"))
                                                (imagemagick :programs
                                                             ("latex" "convert")
                                                             :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                                                             (1.0 . 1.0)
                                                             :latex-compiler
                                                             ("pdflatex -interaction nonstopmode -output-directory %o %f")
                                                             :image-converter
                                                             ("convert -density %D -trim -antialias %f -quality 100 %O")))))
  :config
  ;; (setq org-latex-pdf-process '("latexmk -gg -pdfdvi  %f"))
  ;; (setq org-latex-pdf-process '("latexmk %f"))
  (setq org-latex-pdf-process '("latexmk -gg -pdflua  %f"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-highlight-latex-and-related
        '(latex script entities))
  ;;(setq org-latex-pdf-process '("latexmk -e '$lualatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdflua %f"))
  ;;(setq org-export-in-background t)
  (when (equal system-type 'darwin)
    (setq org-file-apps
          '(("pdf" . "open -a Skim %s"))))
  (when (equal system-type 'gnu/linux)
    (setq org-file-apps
          '(("pdf" . "evince %s"))))
  (add-to-list 'org-latex-classes
               '("lualatex-jlreq"
                 "\\documentclass[]{jlreq}
\\usepackage{luatexja} % ltjclasses, ltjsclasses を使うときはこの行不要
\\usepackage{luatexja-fontspec}
\\usepackage{minted}
\\usepackage[pdfencoding=auto]{hyperref}
\\hypersetup{pdfborder = {0 0 0}}
\\renewcommand{\\listingscaption}{リスト}
\\newcommand{\\uline}[1]{\\underline{#1}}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("jlreq"
                 "\\documentclass[11pt,paper=a4]{jlreq}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\ifdefined\\kanjiskip
  \\usepackage[dvipdfmx]{graphicx}
  \\usepackage[dvipdfmx]{hyperref}
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\usepackage{graphicx}
  \\usepackage{hyperref}
  \\hypersetup{pdfencoding=auto,colorlinks=true}
\\fi"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("lualatex-yukyokasho"
                 "\\documentclass[]{jlreq}
\\usepackage{luatexja} % ltjclasses, ltjsclasses を使うときはこの行不要
\\usepackage{luatexja-fontspec}
\\setmainjfont{YuKyokasho Yoko Medium}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("bxjsarticle"
                 ;; "\\documentclass[twocolumn,autodetect-engine,dvi=dvipdfmx,10pt,a4paper,ja=standard]{bxjsarticle}
                 "\\documentclass[autodetect-engine,dvi=dvipdfmx,10pt,a4paper,ja=standard]{bxjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{siunitx}
% \\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage{fancyhdr}
\\usepackage{listings}
\\usepackage{fancybox}
\\newcommand{\\uline}[1]{\\underline{#1}}
\\ifdefined\\kanjiskip
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=false}
\\else
  \\ifdefined\\XeTeXversion
      \\hypersetup{colorlinks=true}
  \\else
    \\ifdefined\\directlua
      \\hypersetup{pdfencoding=auto,colorlinks=true}
    \\else
      \\hypersetup{unicode,colorlinks=true}
    \\fi
  \\fi
\\fi"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[unicode,dvipdfmx,cjk]{beamer}
\\usepackage{bxdpx-beamer}
\\usepackage{siunitx}
\\usepackage{pxjahyper}
\\usepackage{minijs}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}
\\newcommand{\\uline}[1]{\\underline{#1}}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  (add-to-list 'org-latex-classes
               '("beamer-lualatex"
                 "\\documentclass[unicode,12pt]{beamer}
\\usepackage{luatexja}
\\usepackage[ipaex]{luatexja-preset}
\\renewcommand{\kanjifamilydefault}{\gtdefault}
\\usepackage{bxdpx-beamer}
\\usepackage{siunitx}
\\usepackage{pxjahyper}
\\usepackage{minijs}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}
\\newcommand{\\uline}[1]{\\underline{#1}}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  (add-to-list 'org-latex-classes
               '("jsarticle"
           "\\documentclass[11pt,a4paper]{jsarticle}
\\usepackage{amsmath}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage[dvipdfmx,hiresbb]{graphicx}
\\usepackage[dvipdfmx]{color}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("ieicej"

                 "\\documentclass[paper]{ieicej}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{textcomp}
\\usepackage{latexsym}
\\usepackage{tabularx}
\\usepackage{dcolumn}

\\setcounter{page}{1}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")
                 ("\\paragraph\{%s\}" . "\\paragraph*\{%s\}")
                 ("\\subparagraph\{%s\}" . "\\subparagraph*\{%s\}")))
  (setq org-latex-with-hyperref nil) ;ieicej出力時エラー対策
  (add-to-list 'org-latex-classes
               '("tategaki"

                 "\\documentclass[tate,book,jafontscale=1.3]{jlreq}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{textcomp}
\\usepackage{latexsym}
\\usepackage{tabularx}
\\usepackage{dcolumn}

\\setcounter{page}{1}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")
                 ("\\paragraph\{%s\}" . "\\paragraph*\{%s\}")
                 ("\\subparagraph\{%s\}" . "\\subparagraph*\{%s\}")))
  (add-to-list 'org-latex-classes
               '("jlreq-yoko"

                 "\\documentclass[book,jafontscale=1.3]{jlreq}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{textcomp}
\\usepackage{latexsym}
\\usepackage{tabularx}
\\usepackage{dcolumn}

\\setcounter{page}{1}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")
                 ("\\paragraph\{%s\}" . "\\paragraph*\{%s\}")
                 ("\\subparagraph\{%s\}" . "\\subparagraph*\{%s\}")))
  (add-to-list 'org-latex-classes
               '("luatex-jlreq-tate"
                 "\\documentclass[tate,book,jafontscale=1.3]{jlreq}

\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{textcomp}
\\usepackage{latexsym}
\\usepackage{tabularx}
\\usepackage{dcolumn}
\\usepackage{luatexja-fontspec}

\\setmainfont[Ligatures=TeX]{TeXGyreTermes}
\\setsansfont[Ligatures=TeX]{TeXGyreHeros}

\\setmainjfont[BoldFont=IPAexGothic]{YuKyokasho Medium}
\\setsansjfont{IPAexGothic}

\\newjfontfamily\\jisninety[CJKShape=JIS1990]{IPAexMincho}


\\setcounter{page}{1}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")
                 ("\\paragraph\{%s\}" . "\\paragraph*\{%s\}")
                 ("\\subparagraph\{%s\}" . "\\subparagraph*\{%s\}")))
  (add-to-list 'org-latex-classes
               '("lectureslide"
                 "\\documentclass[unicode,11pt]{beamer}
\\usepackage{bxdpx-beamer}

\\usepackage{xeCJK}
\\usepackage{zxjatype}
\\usepackage{xltxtra} %便利なパッケージ群
\\setCJKmonofont{IPAGothic}
\\usepackage{bm}
\\usepackage{color}
\\usepackage{listings}
\\usepackage{siunitx} %si単位系
\\usepackage{hyperref} %しおり
\\usepackage{ascmac} %角丸の枠
\\usepackage{ulem} %下線
\\usepackage{amsmath,amssymb} %数式，記号
\\usefonttheme[onlymath]{serif}
\\usepackage{minted}
\\usepackage{capt-of} %キャプション
\\usepackage{fancyhdr} %ヘッダ，フッタ
\\usepackage{fancybox} %枠
\\usepackage{tikz} %描画
\\usepackage{graphicx} %画像貼り付け
\\usetheme[progressbar=frametitle]{metropolis}
\\metroset{sectionpage=progressbar, block=fill}
\\setbeamertemplate{navigation symbols}{}
\\setbeamertemplate{footline}[frame number]
\\setbeamertemplate{footline}[page number]
\\setbeamertemplate{itemize items}[triangle]
\\setsansfont[ BoldFont={Fira Sans SemiBold}, ItalicFont={Fira Sans Italic}, BoldItalicFont={Fira Sans SemiBold Italic} ]{Fira Sans}
\\definecolor{myfg}{HTML}{EC9F4C}
\\definecolor{mainbg}{HTML}{3F597C}
\\definecolor{mynormalbg}{HTML}{F2F2F2}
\\definecolor{mynormalfg}{HTML}{4D4D4D}
\\definecolor{myexampletitlefg}{HTML}{6d86ab}
\\setbeamercolor{alerted text}{fg=myfg}
\\setbeamercolor{frameslide}{fg=mynormalbg,bg=mainbg}
\\setbeamercolor{palette primary}{bg=mainbg}
\\setbeamercolor{normal text}{fg=mynormalfg,bg=mynormalbg}
\\setbeamercolor{block title example}{fg=myexampletitlefg}
\\setbeamerfont{alerted text}{series=\\bfseries}

\\setcounter{page}{1}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")
                 ("\\paragraph\{%s\}" . "\\paragraph*\{%s\}")
                 ("\\subparagraph\{%s\}" . "\\subparagraph*\{%s\}")))
  (add-to-list 'org-latex-classes
             '("lectureslide-lualatex"
               "\\documentclass[presentation]{beamer}
[NO-DEFAULT-PACKAGES]
\\usepackage{luatexja}
\\usepackage{textcomp}
\\usepackage{graphicx}
% \\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{hyperref}
\\hypersetup{pdfencoding=auto, linkbordercolor={0 1 0}}
%% Fonts
% mathematical font
\\usepackage{fontspec}
\\usepackage{amsmath, amssymb}
\\usepackage{qtxmath}    % Times (Gyre Termes)
% English
\\setmainfont[BoldFont=TeXGyreHeros, Ligatures=TeX]{TeXGyreTermes}  %Times
\\setsansfont[Ligatures=TeX]{TeXGyreHeros}                          % Helvetica
% Japanese
\\usepackage{luacode}
\\usepackage{luatexja-otf}
\\usepackage[ipaex]{luatexja-preset}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}
% theme
\\usetheme[progressbar=frametitle]{metropolis}
\\metroset{sectionpage=progressbar, block=fill}
\\setbeamertemplate{navigation symbols}{}
\\setbeamertemplate{footline}[frame number]
\\setbeamertemplate{footline}[page number]
\\setbeamertemplate{itemize items}[triangle]
\\setsansfont[ BoldFont={Fira Sans SemiBold}, ItalicFont={Fira Sans Italic}, BoldItalicFont={Fira Sans SemiBold Italic} ]{Fira Sans}
\\definecolor{myfg}{HTML}{EC9F4C}
\\definecolor{mainbg}{HTML}{3F597C}
\\definecolor{mynormalbg}{HTML}{F2F2F2}
\\definecolor{mynormalfg}{HTML}{4D4D4D}
\\definecolor{myexampletitlefg}{HTML}{6d86ab}
\\setbeamercolor{alerted text}{fg=myfg}
\\setbeamercolor{frameslide}{fg=mynormalbg,bg=mainbg}
\\setbeamercolor{palette primary}{bg=mainbg}
\\setbeamercolor{normal text}{fg=mynormalfg,bg=mynormalbg}
\\setbeamercolor{block title example}{fg=myexampletitlefg}
\\setbeamerfont{alerted text}{series=\\bfseries}
%%
\\setbeamercovered{transparent}
\\setbeamertemplate{navigation symbols}{}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; org-export-latex-no-toc
  (defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))
  (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

  ;; reftex with org mode
  ;; (add-hook 'org-mode-hook 'turn-on-reftex)
  ;; (defun org-mode-reftex-setup ()
  ;;   (load-library "reftex")
  ;;   (and (buffer-file-name)
  ;;        (file-exists-p (buffer-file-name))
  ;;        (reftex-parse-all))
  ;;   (define-key org-mode-map (kbd "C-c [") 'reftex-citation))

  )
(leaf ox-taskjuggler
  :custom
  ((org-taskjuggler-process-command . "tj3 --silent --no-color --output-dir %o %f && open %o/Plan.html")))
(leaf ox-gfm
  :straight (ox-gfm :type git :host github :repo "conao3/ox-gfm")
  :require t
  :after org)
(setq org-ditaa-jar-path
      "/usr/local/opt/ditaa/libexec/ditaa-0.11.0-standalone.jar")

(use-package ox-extra
  :straight nil
  :after (org)
  :config
  ;; ignoreタグで見出しを非表示にしつつ内容を表示する
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))
(use-package ob-kotlin
  :after (org))
(leaf ox-asciidoc
  :straight t
  :require t
  :after (org))
(use-package ox-hugo
  :after ox
  :config
  (defun org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
  (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* TODO " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ":END:"
                 "%?\n")          ;Place the cursor here finally
               "\n")))
  (add-to-list 'org-capture-templates
             '("h"                ;`org-capture' binding + h
               "Hugo post"
               entry
               ;; It is assumed that below file is present in `org-directory'
               ;; and that it has a "Blog Ideas" heading. It can even be a
               ;; symlink pointing to the actual location of all-posts.org!
               (file+olp "all-posts.org" "Blog Ideas")
               (function org-hugo-new-subtree-post-capture-template))))


(use-package org-download
  :after org
  :hook ((org-mode . org-download-enable)))
(use-package org-seek
  :commands (org-seek-string org-seek-regexp org-seek-headlines)
;;  :ensure-system-package (rg . ripgrep)
  :config
  (setq org-seek-search-tool 'ripgrep))

(leaf org-pdf*
  ;; :disabled t
  :config
  (use-package org-pdftools
  :after org
  :straight (org-pdftools :type git :host github :repo "fuxialexander/org-pdftools")
  :config (setq org-pdftools-root-dir (concat (getenv "HOME") "/GoogleDrive/Books"))
  (with-eval-after-load 'org
    (org-link-set-parameters "pdftools"
                             :follow #'org-pdftools-open
                             :complete #'org-pdftools-complete-link
                             :store #'org-pdftools-store-link
                             :export #'org-pdftools-export)
    (add-hook 'org-store-link-functions 'org-pdftools-store-link)))
  (use-package org-noter
  :after (org))
  (use-package org-noter-pdftools
  :straight (org-noter-pdftools :type git :host github :repo "fuxialexander/org-pdftools")
  :after (org-noter))
  (leaf pdf-tools
  :straight t
  ;; https://github.com/politza/pdf-tools#installation
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook (pdf-view-mode-hook . (lambda ()
                                (display-line-numbers-mode 0)))
  :custom ((pdf-view-use-scaling . t))
  :config
  (pdf-tools-install)
  (display-line-numbers-mode -1)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1)))




(leaf mu4e
  :straight t
  :load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e)
  :config
  ;;location of my maildir
  (setq mu4e-maildir (expand-file-name "~/.maildir/gmail"))
  ;;command used to get mail
  ;; use this for testing
  ;;(setq mu4e-get-mail-command "true")
  ;; use this to sync with mbsync
  (setq mu4e-get-mail-command "mbsync gmail")

  ;;rename files when moving
  ;;NEEDED FOR MBSYNC
  (setq mu4e-change-filenames-when-moving t)
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; something about ourselves
  (load "~/.mailinfo.el")
  ;; show images
  (setq mu4e-show-images t)
  ;; configuration for sending mail
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)
  (setq mu4e-refile-folder
        (lambda (msg)
          (cond
           ;; messages to the mu mailing list go to the /mu folder
           ((mu4e-message-contact-field-matches msg :to
                                                "mu-discuss@googlegroups.com")
            "/mu")
           ;; messages sent directly to me go to /archive
           ;; also `mu4e-user-mail-address-p' can be used
           ((mu4e-message-contact-field-matches msg :to "me@example.com")
            "/private")
           ;; messages with football or soccer in the subject go to /football
           ((string-match
             "football\\|soccer"              (mu4e-message-field msg :subject))
            "/football")
           ;; messages sent by me go to the sent folder
           ;;((find-if
           ;;  (lambda (addr)
           ;;  (mu4e-message-contact-field-matches msg :from addr))
           ;;     mu4e-user-mail-address-list)
           ;;  mu4e-sent-folder)
           ;; everything else goes to /archive
           ;; important to have a catch-all at the end!
           (t  "/archive"))))
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
  ;; save attachment to my desktop (this can also be a function)
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-maildir-shortcuts
        '( ("/inbox"   . ?i)
           ("/sent"    . ?s)
           ("/trash"   . ?t)
           ("/archive" . ?a))))

(leaf company
  :straight t
  :diminish company-mode
  :bind ((:company-mode-map
          ("C-M-i" . company-indent-or-complete-common))
         (:company-active-map
          ("C-n"   . company-select-next)
          ("C-p"   . company-select-previous)
          ("C-s"   . company-filter-candidates)
          ("C-i"   . company-complete-selection))
         (:company-search-map
          ("C-n"   . company-select-next)
          ("C-p"   . company-select-previous)))
  :hook ((emacs-lisp-mode-hook   . company-mode)
         (c-mode-hook            . company-mode)
         (shell-script-mode-hook . company-mode)
         (sh-mode-hook           . company-mode)
         (shell-mode-hook        . company-mode)
         (org-mode-hook          . company-mode)
         (lisp-mode-hook         . company-mode))
  :custom
  ((company-idle-delay . 0.2)
   (company-minimum-prefix-length . 2)
   (company-selection-wrap-around . t)
   (company-lsp-async . t)
   (company-lsp-cache-candidates . nil)
   (company-lsp-enable-recompletion . t)
   (company-lsp-enable-snippet . t))
  :config
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(leaf yatex
  :straight t
  :mode
  (("\\.tex$" . yatex-mode)
   ("\\.ltx$" . yatex-mode)
   ("\\.cls$" . yatex-mode)
   ("\\.sty$" . yatex-mode)
   ("\\.clo$" . yatex-mode)
   ("\\.bbl$" . yatex-mode))
  :config
  (setq YaTeX-inhibit-prefix-letter t)
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-latex-message-code 'utf-8)
  (setq YaTeX-use-LaTeX2e t)
  (setq YaTeX-use-AMS-LaTeX t)
  (setq YaTeX-dvi2-command-ext-alist
        '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|atril\\|xreader\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|MicrosoftEdge\\|microsoft-edge\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
  ;;(setq tex-command "ptex2pdf -u -l -ot '-synctex=1'")
  ;;(setq tex-command "lualatex -synctex=1")
  ;;(setq tex-command "latexmk")
  (setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -shell-escape -norc -gg -pdfdvi")
  ;;(setq tex-command "latexmk -e '$lualatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
  (setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
  (setq makeindex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
  (setq dvi2-command "open -a Skim")
  ;;(setq dvi2-command "open -a Preview")
  ;;(setq dvi2-command "open -a TeXShop")
  ;;(setq dvi2-command "/Applications/TeXworks.app/Contents/MacOS/TeXworks")
  ;;(setq dvi2-command "/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only")
  (setq tex-pdfview-command "open -a Skim")
  ;;(setq tex-pdfview-command "open -a Preview")
  ;;(setq tex-pdfview-command "open -a TeXShop")
  ;;(setq tex-pdfview-command "/Applications/TeXworks.app/Contents/MacOS/TeXworks")
                                        ;(setq tex-pdfview-command "/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only")
  (setq dviprint-command-format "open -a \"Adobe Acrobat Reader DC\" `echo %s | gsed -e \"s/\\.[^.]*$/\\.pdf/\"`")
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (auto-fill-mode -1)))
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (reftex-mode 1)
               (define-key reftex-mode-map
                 (concat YaTeX-prefix ">") 'YaTeX-comment-region)
               (define-key reftex-mode-map
                 (concat YaTeX-prefix "<") 'YaTeX-uncomment-region))))
;; for yatex
(when (equal system-type 'darwin)
  (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:/Applications/Skim.app/Contents/SharedSupport:$PATH" t)
  (setq exec-path (append '("/usr/local/bin" "/Library/TeX/texbin" "/Applications/Skim.app/Contents/SharedSupport") exec-path)))

(leaf php-mode
  :straight t
  :mode (("\\.php\\'" . php-mode))
  :custom
  ((php-manual-url . 'ja)))
(leaf ac-php
  :straight t
  :after php-mode)
(leaf flycheck-phpstan
  :straight t
  :hook (php-mode-hook . (lambda ()
                           (require 'flycheck-phpstan)
                           (flycheck-mode t))))
(use-package company-php
  :after (ac-php)
  :straight t
  :hook (php-mode-hook . (lambda ()
                           ;; Enable company-mode
                           (company-mode t)
                           ;; (require 'company-php)

                           ;; Enable ElDoc support (optional)
                           (ac-php-core-eldoc-setup)

                           (set (make-local-variable 'company-backends)
                                '((company-ac-php-backend company-dabbrev-code)
                                  company-capf company-files)))))

(leaf rainbow-mode
  :straight t)

(leaf lsp-mode
  :commands (lsp lsp-deferred)
  :custom ((lsp-prefer-capf . t)
           (lsp-keymap-prefix . "C-c C-l"))
  :hook ((cc-mode     . lsp-deferred)
         (lsp-mode-hook . lsp-enable-which-key-integration))
  :require t
  :init (setq read-process-output-max (* 1024 1024))
  (setq garbage-collection-messages t))

(leaf lsp-python-ms
  :disabled t
  :straight t
  :require t
  :custom
  ((lsp-python-ms-python-executable-cmd . "python3"))
  :hook ((python-mode-hook . (lambda ()
                                (require 'lsp-python-ms)
                                (when (file-exists-p
                                       (concat (projectile-project-root buffer-file-name) ".venv/"))
                                  (setq lsp-python-ms-extra-paths
                                        (vector
                                         (format
                                          "%s/site-packages"
                                          (car
                                           (last (directory-files
                                                  (concat
                                                   (projectile-project-root buffer-file-name)
                                                   ".venv/lib/")
                                                  t))))))
                                  (message "lsp-python-ms-extra-paths `%s'" lsp-python-ms-extra-paths))
                                (lsp-deferred))))
  :config
  (setq lsp-python-ms-auto-install-server t)
  (add-hook 'python-mode-hook #'lsp-deferred) ; or lsp
  )
(leaf lsp-pyright
  :straight t
  :hook ((python-mode-hook . (lambda ()
                         (require 'lsp-pyright)
                         (when (file-exists-p
                                (concat (projectile-project-root buffer-file-name) ".venv/"))
                           (setq lsp-pyright-extra-paths
                                 (vector
                                  (format
                                   "%s/site-packages"
                                   (car
                                    (last (directory-files
                                           (concat
                                            (projectile-project-root buffer-file-name)
                                            ".venv/lib/")
                                           t))))))
                           (message "lsp-pyright-extra-paths `%s'" lsp-pyright-extra-paths))
                         (lsp-deferred))))
  
  :config
  (dolist (dir '(
                 "[/\\\\]\\.venv$"
                 "[/\\\\]\\.mypy_cache$"
                 "[/\\\\]__pycache__$"
                 ))
    (push dir lsp-file-watch-ignored))
  )
(leaf poetry
  :straight t
  :require t)

(leaf pipenv
  :disabled t
  :hook (python-mode-hook . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; optionally
(leaf lsp-ui
  :straight t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :commands lsp-ui-mode
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable                  . t)
  (lsp-ui-doc-header                  . t)
  (lsp-ui-doc-include-signature       . t)
  (lsp-ui-doc-position                . 'bottom) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width               . 60)
  (lsp-ui-doc-max-height              . 20)
  (lsp-ui-doc-use-childframe          . t)
  (lsp-ui-doc-use-webkit              . nil)

  (lsp-ui-sideline-enable             . t)
  (lsp-ui-sideline-ignore-duplicate   . t)
  (lsp-ui-sideline-show-symbol        . t)
  (lsp-ui-sideline-show-hover         . t)
  (lsp-ui-sideline-show-diagnostics   . t)
  (lsp-ui-sideline-show-code-actions  . t)
  :bind `((:lsp-ui-mode-map
          ("M-." . lsp-ui-peek-find-definitions)
          ("M-?" . lsp-ui-peek-find-references)
          (,(concat lsp-keymap-prefix " t") . lsp-ui-doc-focus-frame)))
)

(leaf lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))
;; optionally if you want to use debugger
(leaf lsp-java
  :straight t
  :require t
  :hook (java-mode-hook . lsp-deferred)
  :bind ((:lsp-mode-map
          ("M-." . lsp-find-definition))))
(leaf dap-mode
  :straight t
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (leaf dap-java
    :require t
    :after (lsp-java)))

(leaf hydra :straight t)
(leaf projectile-ripgrep :straight t)


;; ;;git clone git@github.com:rswarbrick/picasm.git ~/.emacs.d/lisp/picasm
;; (use-package picasm
;;   :load-path "~/.emacs.d/lisp/picasm/")

(leaf rustic :straight t
  :hook (rust-mode-hook . rustic-mode)
  :require t
  :init
  (setq rustic-lsp-server 'rust-analyzer))

(leaf go-mode
  :straight t
  :after lsp-mode
   :hook (go-mode-hook . lsp-defferd))

(leaf android-mode
  :straight t
  :disabled t)

(leaf ccls :straight t
  :after lsp-mode
;;  :ensure-system-package ccls
  :hook ((c-mode-hook c++-mode-hook objc-mode-hook) .
         (lambda () (require 'ccls) (lsp-deferred)))
  :config
  (when (eq system-type 'darwin)
    (when (executable-find "/usr/local/opt/ccls/bin/ccls")
      (setq ccls-executable "/usr/local/opt/ccls/bin/ccls"))
    (when (executable-find "/opt/homebrew/opt/ccls/bin/ccls")
      (setq ccls-executable "/opt/homebrew/opt/ccls/bin/ccls"))
    (when (executable-find "/opt/local/bin/ccls-clang-11")
        (setq ccls-executable "/opt/local/bin/ccls-clang-11"))
    ;; (setq ccls-initialization-options
    ;;       '(:clang (:extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
    ;;                             "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
    ;;                             "-isystem/usr/local/include"
    ;;                             "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/12.0.0/include"
    ;;                             "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
    ;;                             "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"
    ;;                             "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks"]
    ;;                            :resourceDir "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/12.0.0")))
    ))



(leaf smartparens
  :straight t
  :diminish t
  :require smartparens-config
  :hook (after-init-hook . smartparens-global-mode))

(leaf kotlin-mode
  :straight t
  :mode (("\\.kt\\'" . kotlin-mode)))

(leaf whitespace
  :require t
  :diminish global-whitespace-mode
  :config
  (set-face-foreground 'whitespace-space nil)
  (set-face-background 'whitespace-space "gray33")
  (setq whitespace-style '(face
                           ;; trailing
                           ;; tabs
                           spaces
                           ;; empty
                           ;; space-mark
                           ;; tab-mark
                           ))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (global-whitespace-mode 1))


(leaf plantuml-mode
  :straight t
  :config
  (when (eq system-type 'darwin)
    (setq plantuml-jar-path
          "/usr/local/opt/plantuml/libexec/plantuml.jar")))

(leaf htmlize :straight t)
(leaf adoc-mode :straight t)
(leaf pandoc :straight t)
(leaf graphviz-dot-mode :straight t)
(leaf editorconfig
  :straight t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))
(use-package easy-hugo
  :disabled t
  :config
  (setq easy-hugo-org-header t)
  (setq easy-hugo-default-ext ".org"))
(leaf npm-mode
  :disabled t
  :straight t
;;  :ensure-system-package npm
)
(leaf autodisass-java-bytecode
  :straight t)

(leaf google-c-style
  :straight t
  :commands
  (google-set-c-style))
(leaf regex-tool :straight t)

(leaf solarized-theme
  :straight t
  :when (window-system)
  :config
  ;; (load-theme 'solarized-dark t)
  (load-theme 'solarized-iceberg-dark t))
(leaf markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (when (eq window-system 'ns)
        (set-face-attribute 'markdown-table-face nil
                            :family "IPAGothic")))
(leaf docker :straight t)
(leaf docker-compose-mode :straight t)
(leaf review-mode
  :straight t
  :mode (("\\.re\\'" . review-mode)))
(leaf csv-mode :straight t)

(leaf org-re-reveal
  :straight t
  :after org)
(leaf org-gcal
  :disabled t
  :if (file-exists-p "~/Dropbox/org/googlecalendar/org-gcal-config.el")
  :straight t
  :after org
  :require t
  :custom
  ((org-gcal-down-days . 180)
   (org-gcal-up-days . 180))
  :config
  (load "~/Dropbox/org/googlecalendar/org-gcal-config.el"))

(leaf flycheck :straight t)
(leaf gnuplot :straight t)

(leaf *gdb
  :config
  ;;; GDB 関連
;;; 有用なバッファを開くモード
(setq gdb-many-windows t)

;;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))

;;; I/O バッファを表示
(setq gdb-use-separate-io-buffer t)

;;; t にすると mini buffer に値が表示される
(setq gud-tooltip-echo-area nil)

;;; バックアップファイルを作成しない
(setq make-backup-files t))

(leaf asm-mode
  :hook ((asm-mode-set-comment-hook . (lambda ()
                                        (setq asm-comment-char ?#)))))
(leaf ssh-config-mode
  :straight t)
(leaf dockerfile-mode
  :straight t
  :require t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
(leaf git-modes
  :straight t
  :require t)
(leaf go-mode
  :straight t
  :hook ((go-mode-hook . lsp-deferred)))
(leaf groovy-mode
  :straight t)

(leaf server
  :commands (server-running-p)
  :hook
  (emacs-startup-hook . (lambda ()
                          (unless (server-running-p)
                            (server-start))))
  )

;; https://gist.github.com/tek-nishi/a7fc3933be5e62c7eeaa
(defun my-insert-newline-and-indent(arg)
  "カーソル行の上や下に一行挿入してインデント(前置引数が４だと上の行に挿入)"
  (interactive "p")
  (let ((p (if (eq arg 4)
               1
             2)))
    (move-beginning-of-line p)
    (open-line 1)
    (indent-for-tab-command)))

;;from https://uwabami.github.io/cc-env/Emacs.html
(defun my:make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg
        (progn
          (setq arg 0)
          (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))
;;
(defun my:buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my:make-scratch 0) nil)
                        t))))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら
          ;; *scratch* バッファを新しく作る.
          (function
           (lambda ()
             (unless (member "*scratch*" (my:buffer-name-list))
               (my:make-scratch 1)))))

(put 'narrow-to-region 'disabled nil)
(setq ispell-program-name "hunspell")
(setq ispell-really-hunspell t)



(dolist (file (cddr (directory-files (concat
                                      user-emacs-directory
                                      "lisp/"))))
  (load-file (concat user-emacs-directory "lisp/" file)))


;; 読み込み専用で開く設定を持ったクラスを定義
(dir-locals-set-class-variables
 'read-only
 '((nil .((buffer-read-only . t)))))
;; クラスをディレクトリに関連づける
(dolist (dir(mapcar (lambda (str)
                      (format
                       "%spackages/%s/straight/repos/"
                       user-emacs-directory
                       str))
                    (cddr
                     (directory-files
                      (concat
                       user-emacs-directory
                       "packages/")))))
  (dir-locals-set-directory-class (file-truename dir) 'read-only))

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
