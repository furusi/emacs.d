;;; init.el --- my init script

;;; Commentary:

;;; Code:

(tool-bar-mode -1)
(show-paren-mode t)

;; 絵文字のフォント設定
(when window-system
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-use-jump nil)
 '(backup-directory-alist '((".*" . "~/.ehist")))
 '(comment-style 'multi-line)
 '(company-idle-delay 0.2)
 '(company-lsp-async t)
 '(company-lsp-cache-candidates nil)
 '(company-lsp-enable-recompletion t)
 '(company-lsp-enable-snippet t)
 '(context-skk-context-check-hook
   '(context-skk-out-of-string-or-comment-in-programming-mode-p context-skk-on-keymap-defined-area-p context-skk-in-read-only-p
                                                                (lambda nil
                                                                  (looking-at "\\*+ "))
                                                                (lambda nil
                                                                  (looking-at "\\#\\+BEGIN_"))))
 '(custom-enabled-themes nil)
 '(dimmer-exclusion-regexp "^\\\\*helm\\\\|^ \\\\*Minibuf\\\\|^\\\\*Calendar\"")
 '(dimmer-fraction 0.3)
 '(dired-dwim-target t)
 '(easy-kill-alist
   '((84 string-up-to-char-backward "")
     (116 string-to-char-backward "")
     (70 string-up-to-char-forward "")
     (102 string-to-char-forward "")
     (62 buffer-after-point "")
     (60 buffer-before-point "")
     (98 buffer "")
     (36 forward-line-edge "")
     (94 backward-line-edge "")
     (119 word " ")
     (115 sexp "
")
     (108 list "
")
     (102 filename "
")
     (100 defun "

")
     (68 defun-name " ")
     (101 line "
")
     (98 buffer-file-name nil)))
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elpy-rpc-python-command "python3")
 '(eval-expression-print-length nil)
 '(helm-candidate-number-limit 300)
 '(helm-completion-style 'emacs)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(leaf-alias-keyword-alist '((:ensure . :straight)))
 '(lsp-python-ms-python-executable-cmd "python3")
 '(magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
 '(org-babel-java-compiler "javac -encoding UTF-8")
 '(org-babel-load-languages '((emacs-lisp . t) (C . t) (dot . t) (java . t)))
 '(org-export-backends '(ascii html icalendar latex md odt taskjuggler))
 '(org-journal-date-format "%A, %d %B %Y" t)
 '(org-latex-compiler "lualatex")
 '(org-latex-default-class "bxjsarticle")
 '(org-latex-listings 'minted)
 '(org-latex-listings-options
   '(("frame" "single")
     ("basicstyle" "{\\ttfamily\\scriptsize}")
     ("numbers" "left")
     ("commentstyle" "{\\ttfamily\\scriptsize}")
     ("breaklines" "true")
     ("showstringspaces" "false")))
 '(org-latex-minted-langs
   '((rust "rust")
     (emacs-lisp "common-lisp")
     (cc "c++")
     (cperl "perl")
     (shell-script "bash")
     (caml "ocaml")
     (bash "bash")
     (conf "ini")))
 '(org-link-file-path-type 'relative)
 '(org-list-allow-alphabetical t)
 '(org-preview-latex-process-alist
   '((dvipng :programs
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
                  ("convert -density %D -trim -antialias %f -quality 100 %O"))))
 '(org-return-follows-link t)
 '(org-rst-headline-underline-characters '(45 126 94 58 39 32 95))
 '(org-src-lang-modes
   '(("arduino" . arduino)
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
 '(org-src-preserve-indentation t)
 '(org-structure-template-alist
   '(("n" . "notes")
     ("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
 '(org-taskjuggler-process-command
   "tj3 --silent --no-color --output-dir %o %f && open %o/Plan.html")
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(easy-hugo lsp-mode use-package-ensure-system-package spinner company yasnippet all-the-icons ob-kotlin ace-jump-mode ace-isearch helm-swoop helm-migemo migemo gnu-elpa-keyring-update rustic review-mode pandoc ox-epub ob-browser htmlize adoc-mode ox-asciidoc ox-hugo org company-arduino arduino-mode pandoc-mode lorem-ipsum undo-propose 0x0 all-the-icons-ivy groovy-mode ob-rust multi-term back-button jedi jedi-core lsp-java-treemacs dap-java flycheck-rust cargo racer howm counsel-tramp dropbox editorconfig editorconfig-generate ox-pandoc c-eldoc ggtags graphviz-dot-mode kotlin-mode php-mode visual-regexp-steroids omnisharp dap-mode treemacs lsp-java ccls zenburn-theme yatex yasnippet-snippets which-key web-mode use-package undohist undo-tree sudo-edit spacemacs-theme smartparens smart-mode-line slime rust-mode restart-emacs poet-theme plantuml-mode pipenv ox-rst ox-reveal org-plus-contrib org-mobile-sync org-journal org-ac nim-mode magit-popup magit lsp-ui keyfreq helm gradle-mode exec-path-from-shell elpy dimmer ddskk company-web company-shell company-php company-lsp company-jedi company-irony auto-save-buffers-enhanced))
 '(php-manual-url 'ja)
 '(picasm-db-file "~/.emacs.d/lisp/picasm/picasm-db.el")
 '(plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar" t)
 '(python-shell-interpreter "python3")
 '(recentf-auto-cleanup 'never)
 '(recentf-exclude
   '("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" "/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/" "/\\.emacs\\.d/elpa/"))
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 2000)
 '(rst-compile-toolsets
   '((html "rst2html.py" ".html" nil)
     (latex "rst2latex.py" ".tex" nil)
     (newlatex "rst2newlatex" ".tex" nil)
     (pseudoxml "rst2pseudoxml.py" ".xml" nil)
     (xml "rst2xml.py" ".xml" nil)
     (pdf "rst2pdf" ".pdf" "-s ja")
     (s5 "rst2s5.py" ".html" nil)))
 '(slime-auto-start 'ask)
 '(slime-company-completion 'fuzzy)
 '(slime-complete-symbol*-fancy t)
 '(sp-escape-quotes-after-insert nil)
 '(use-package-compute-statistics t)
 '(vc-follow-symlinks t)
 '(zenburn-scale-org-headlines t)
 '(zenburn-scale-outline-headlines t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-lsp-flycheck-warning-unnecessary-face ((t (:background "gray68"))) t)
 '(lsp-ui-sideline-code-action ((t (:foreground "yellow3"))))
 '(org-table ((t (:foreground "cornflower blue")))))

(leaf initchart
  :disabled t
  :straight (initchart :type git :host github :repo "yuttie/initchart")
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
  (setq exec-path-from-shell-check-startup-files nil)
  (add-to-list 'exec-path-from-shell-variables "PYTHONPATH")
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

(when (equal system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e/")
  (when (eq window-system 'ns)
        ;; 游教科書体
        ;; (set-face-attribute 'default nil
        ;;                     :family "YuKyokasho Yoko")
        ;; 源ノ角ゴシック
        (set-face-attribute 'default nil
                            :family "Noto Sans Mono CJK JP")))
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

(leaf sudo-edit :straight t)
(leaf projectile
  :bind ((:projectile-mode-map
          ("C-c p" . projectile-command-map)))
  :straight t
  :custom
  ((projectile-indexing-method . 'hybrid)
   (projectile-sort-order . 'recently-active))
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
  ((skk-share-private-jisyo . t)
   (skk-isearch-start-mode . 'latin); isearch で skk の初期状態
   )
  :init
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
  )


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
  :config
  (setq magit-diff-refine-hunk 'all)
  ;; ediff時にorgファイルを全て表示する
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'org-show-all)))
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
               "/usr/local/opt/cmigemo/share/migemo/utf-8/migemo-dict")
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


(leaf undohist :straight t
  :require t
  :config
  (undohist-initialize)
  ;;; 永続化を無視するファイル名の正規表現
  (setq undohist-ignored-files
        '("/tmp/" "COMMIT_EDITMSG")))

(leaf undo-tree :straight t
  :init
  (global-undo-tree-mode t))


(leaf auto-save-buffers-enhanced
  :straight t
  :config
  ;; 1秒後に保存
  (setq auto-save-buffers-enhanced-interval 1)
  (auto-save-buffers-enhanced t)
  ;; Wroteのメッセージを抑制
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  ;; tramp mode時の自動保存を抑制
  (setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "^/scp:" "/sudo:" "/multi:" ".*.gpg$")))

(leaf real-auto-save
  :disabled t
  :straight t
  :require t
  :custom ((real-auto-save-interval . 0.5))
  :hook ((org-mode-hook prog-mode-hook) . real-auto-save-mode))

;; helm
(leaf *helm
  :config
  (leaf helm
    :straight t
    :require helm-config
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

(leaf *counsel
  :disabled t
  :config
  (leaf counsel
    :straight t
    :require ivy
    :custom (((ivy-use-virtual-buffers . t)))
    :bind (("M-x" . counsel-M-x)
           ("C-x C-b" . counsel-ibuffer)
           ("C-x b" . ivy-switch-buffer)
           ("C-x C-f" . counsel-find-file)
           ("M-y" . counsel-yank-pop)
           ("C-x c i" . counsel-imenu)
           ("C-x j" . counsel-recentf))
    :config
    (ivy-mode 1))
  (leaf counsel-projectile
    :straight t
    :config
    (counsel-projectile-mode))
  (leaf lsp-ivy
    :straight t))


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
  :config
  ;; 3つの表示方法どれか1つ選ぶ
  (which-key-setup-side-window-bottom)    ;ミニバッファ
  ;; (which-key-setup-side-window-right)     ;右端
  ;; (which-key-setup-side-window-right-bottom) ;両方使う
  (which-key-mode 1))

;;;yasnippet
(leaf yasnippet
  :straight t
  :config
  (yas-global-mode 1))
(leaf yasnippet-snippets :straight t)


(leaf keyfreq :straight t
  :config
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
  :straight t
  :mode (("\\.gradle$" . gradle-mode)))


(leaf slime
  :straight slime-company
  :if (file-exists-p "~/.roswell/helper.el")
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
    :commands (org-show-all)
    :mode (("\\.org$" . org-mode))
    :straight org-plus-contrib
    :bind (("\C-cc" . org-capture)
           ("\C-cl" . org-store-link)
           ("\C-ca" . org-agenda)
           ("\C-cb" . org-iswitchb)
           (:org-mode-map
            ("C-c C-\'" . org-insert-structure-template)))
    :init
    (setq org-directory
          (expand-file-name
           (if (file-exists-p "~/git/notes")
               "~/git/notes"
             "~/org")))

    :custom
    ((org-preview-latex-default-process . 'dvisvgm))
    :config
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
           (concat org-directory "/agenda/")
           (concat org-directory "/task.org")
           (concat org-directory "/habit.org")
           (concat org-directory "/event.org")
           (concat org-directory "/inbox.org")
           (concat org-directory "/productivity.org")
           (concat org-directory "/org-ical.org")
           (concat org-directory "/notes/")
           (concat org-directory "/googlecalendar/")))
    (setq org-refile-targets
          '((org-agenda-files :maxlevel . 2)))
    (setq org-tag-alist
          '(("ignore" . ?i) ("@OFFICE" . ?o) ("@HOME" . ?h) ("SHOPPING" . ?s)
            ("MAIL" . ?m) ("PROJECT" . ?p) ("備忘録" . ?b)))
    (setq org-capture-templates
          `(("i" "インボックス" entry
             (file ,(concat org-directory "/inbox.org"))
             "* %? %i\n %U\n")
            ;; ("h" "定期的にやること" entry
            ;;  (file ,(concat org-directory "/habit.org"))
            ;;  "* %?\n %U\n")
            ("t" "タスク" entry
             (file ,(concat org-directory "/task.org"))
             "* TODO %? %i\n %U\n")
            ("e" "イベント" entry
             (file ,(concat org-directory "/event.org"))
             "* EVENT %? %i\n %a\n %U\n")
            ("n"
             "ノート(本文から書く)"
             entry
             (file+headline, (concat org-directory "/notes.org") "MEMO")
             "* %U \n%?")
            ("N"
             "ノート(見出しから書く)"
             entry
             (file+headline, (concat org-directory "/notes.org") "MEMO")
             "* %U %?\n\n\n")
            ("r" "読みかけ(リンク付き)" entry
             (file ,(concat org-directory "/reading.org"))
             "* %?\n %a\n %U\n")
            ("m"
             "みんなで会議"
             entry
             (file+olp+datetree (concat org-directory "/minutes.org") "会議")
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
             (file+headline (concat org-directory "/gtd.org") "GTD")
             "** TODO %T %?\n   Entered on %U    %i\n"
             :empty-lines 1)
            ("i"
             "itemのテスト"
             item
             (file+headline (concat org-directory "/gtd.org") "GTD")
             "** TODO %T %?\n   Entered on %U    %i\n"
             :empty-lines 1)
            ("z"
             "'あれ'についてのメモ"
             entry
             (file+headline , (concat org-directory "/notes.org") "MEMO")
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
    ;; カーソルが見出しにある場合latinモードになる
    (custom-add-frequent-value 'context-skk-context-check-hook
                               #'org-at-heading-p)
    (custom-reevaluate-setting 'context-skk-context-check-hook)

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
  (leaf org-mu4e
    :disabled t
    :straight t
    :load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e"
    :after (org)
    :config
    ;;store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil))
  (leaf ox-rst
    :straight t
    :after (org))
  (leaf ox-hugo
    :straight t
    :after org)
  (leaf ob-browser
    :straight t
    :after org)
  (leaf ox-epub
    :straight t
    :after org)
  )





(use-package org-journal
  :after org
  :commands org-journal-new-entry
  :custom
  (org-journal-dir (concat org-directory "journal"))
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-file-header "# -*- mode: org-journal; -*-"))



;; Org Mode LaTeX Export

(use-package ox-eldoc
  :straight nil
  :defer t
  :config
  (defadvice org-eldoc-documentation-function (around add-field-info activate)
    (or
     (ignore-errors (and (not (org-at-table-hline-p))
                         (org-table-field-info nil)))
     ad-do-it))

  (add-hook 'org-mode-hook 'eldoc-mode)

  (eldoc-add-command-completions
   "org-table-next-" "org-table-previous" "org-cycle"))


(leaf ox-latex
  :require t
  :straight nil
  :after (org)
  :custom ((org-latex-minted-options . '(("frame" "single")
                                         ("breaklines" "")
                                         ("fontsize" "\\footnotesize"))))
  :config
  (setq org-latex-default-class "bxjsarticle")
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
(use-package ox-asciidoc
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

(leaf ox-pandoc
  ;;  :ensure-system-package pandoc
  :if (or (file-exists-p "/usr/local/bin/pandoc")
          (file-exists-p "/opt/local/bin/pandoc"))
  :after ox)
(use-package org-download
  :after org
  :hook ((org-mode . org-download-enable)))
(use-package org-seek
  :commands (org-seek-string org-seek-regexp org-seek-headlines)
;;  :ensure-system-package (rg . ripgrep)
  :config
  (setq org-seek-search-tool 'ripgrep))

(leaf org-pdf*
  :disabled t
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
  :disabled t
  :straight t
  ;; https://github.com/politza/pdf-tools#installation
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install)
  (display-line-numbers-mode -1)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1)))


(use-package org-roam
  :straight (org-roam :type git :host github :repo "org-roam/org-roam")
  :custom
  (org-roam-directory "~/Dropbox/org/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

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
  :config
  (setq company-idle-delay 0) ; 遅延なしにすぐ表示
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
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
  :mode (("\\.php\\'" . php-mode)))
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

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom ((lsp-prefer-capf t))
  :hook ((cc-mode     . lsp-deferred)
         ;; (python-mode . lsp-deferred)
         ))

(leaf lsp-python-ms
  :straight t
  :require t
  :hook (('python-mode-hook . (lambda ()
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

(use-package lsp-python-ms
  :disabled t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook  (python-mode . (lambda ()
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
                         ;; or lsp
                         (lsp-deferred)))
  )
(leaf pipenv
  :disabled t
  :hook (python-mode-hook . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; optionally
(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width 60)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :after lsp-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
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

(leaf android-mode
  :straight t
  :disabled t)

(leaf ccls :straight t
  :commands ccls
;;  :ensure-system-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (when (eq system-type 'darwin)
    (when (executable-find "/usr/local/opt/ccls/bin/ccls")
      (setq ccls-executable "/usr/local/opt/ccls/bin/ccls"))
    (setq ccls-initialization-options
          '(:clang (:extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                "-isystem/usr/local/include"
                                "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.0/include"
                                "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
                                "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"
                                "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks"]
                               :resourceDir "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.0")))))



(leaf smartparens-config
  :straight smartparens
  :require t
  :hook (after-init-hook . smartparens-global-mode))

(leaf kotlin-mode
  :straight t
  :mode (("\\.kt\\'" . kotlin-mode)))

(leaf whitespace
  :require t
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
  :disabled t
  :straight t
  :config
  (load-theme 'solarized-dark t))
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
  :if (file-exists-p "~/Dropbox/org/googlecalendar/org-gcal-config.el")
  :straight t
  :after org
  :require t
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
