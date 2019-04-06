;;; init.el --- my init script

;;; Commentary:

;;; Code:

(recentf-mode 1)
(setq recentf-max-menu-items 30)
;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key "\C-x\ \C-r" 'helm-recentf)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Orgを追加
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(eval-when-compile
  (unless (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package))
  )

(show-paren-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.ehist"))))
 '(dimmer-exclusion-regexp "^\\\\*helm\\\\|^ \\\\*Minibuf\\\\|^\\\\*Calendar\"")
 '(dimmer-fraction 0.3)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eval-expression-print-length nil)
 '(inhibit-startup-screen t)
 '(magit-display-buffer-function (quote magit-display-buffer-fullframe-status-v1))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/task.org" "~/Dropbox/org/notes.org" "~/Dropbox/org/habit.org" "~/Dropbox/org/event.org" "~/Dropbox/org/inbox.org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (C . t) (dot . t))))
 '(org-export-backends (quote (ascii html icalendar latex odt taskjuggler)))
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-dir "~/Dropbox/org/journal")
 '(org-latex-default-class "bxjsarticle")
 '(org-latex-listings t)
 '(org-latex-listings-options
   (quote
    (("frame" "single")
     ("basicstyle" "{\\ttfamily\\scriptsize}")
     ("numbers" "left")
     ("commentstyle" "{\\gtfamily\\scriptsize}")
     ("breaklines" "true")
     ("showstringspaces" "false"))))
 '(org-latex-minted-options (quote (("frame" "single") ("linenos" "true"))))
 '(org-latex-pdf-process (quote ("latexmk -gg -pdfdvi  %f")))
 '(org-return-follows-link t)
 '(org-rst-headline-underline-characters (quote (45 126 94 58 39 32 95)))
 '(org-src-lang-modes
   (quote
    (("html" . web)
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
     ("asm" . picasm))))
 '(org-structure-template-alist
   (quote
    (("n" . "notes")
     ("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))))
 '(org-taskjuggler-process-command
   "tj3 --silent --no-color --output-dir %o %f && open %o/Plan.html")
 '(package-selected-packages
   (quote
    (ccls zenburn-theme yatex yasnippet-snippets which-key web-mode use-package undohist undo-tree sudo-edit spacemacs-theme smartparens smart-mode-line slime rust-mode restart-emacs poet-theme plantuml-mode pipenv ox-rst ox-reveal org-plus-contrib org-mobile-sync org-journal org-ac nim-mode magit-popup magit lsp-ui keyfreq htmlize helm graphviz-dot-mode gradle-mode exec-path-from-shell elpy dimmer ddskk company-web company-shell company-php company-lsp company-jedi company-irony auto-save-buffers-enhanced)))
 '(picasm-db-file "~/.emacs.d/lisp/picasm/picasm-db.el")
 '(rst-compile-toolsets
   (quote
    ((html "rst2html.py" ".html" nil)
     (latex "rst2latex.py" ".tex" nil)
     (newlatex "rst2newlatex" ".tex" nil)
     (pseudoxml "rst2pseudoxml.py" ".xml" nil)
     (xml "rst2xml.py" ".xml" nil)
     (pdf "rst2pdf" ".pdf" "-s ja")
     (s5 "rst2s5.py" ".html" nil))))
 '(zenburn-scale-org-headlines t)
 '(zenburn-scale-outline-headlines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'load-path "~/.emacs.d/conf")
(load "org-init")
(load "yatex-init")
(load "mu4e-init")
(load "magit-init")
(define-key global-map (kbd "C-c t l") 'toggle-truncate-lines)
(setq-default indent-tabs-mode nil)
(use-package restart-emacs
  :ensure t)

(use-package gradle-mode
  :ensure t)

(set-face-attribute 'default nil :height 140)



;; Emacs起動時にrst.elを読み込み
(use-package rst
  :ensure t
  :bind (
         :map rst-mode-map
              ("M-RET" . rst-insert-list))
  :config
  (when (eq system-type 'darwin)
    (setq rst-pdf-program "open -a Skim")
    (setq rst-slides-program "open -a Firefox")
    )
  )
;; 拡張子の*.rst, *.restのファイルをrst-modeで開く
(setq auto-mode-alist
      (append '(( "\\.rst$"  . rst-mode)
                ( "\\.rest$" . rst-mode)
                ("\\.php$"   . web-mode)) auto-mode-alist))
;; 背景が黒い場合はこうしないと見出しが見づらい
;; (setq frame-background-mode 'dark)
;; 全部スペースでインデントしましょう
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))

(use-package gradle-mode)

(use-package eww
  :config
  (setq eww-search-prefix "http://www.google.co.jp/search?q=")
  )

;;(setq coding-system-for-read 'utf-8)
;;(load-theme 'atom-dark t)
;;(load-theme 'leuven t)
(use-package poet-theme
  :ensure t
  :disabled t
  :config
  (load-theme 'poet t))
(use-package zenburn-theme
  :ensure t
  :disabled t
  :config
  (load-theme 'zenburn t))
(use-package spacemacs-theme
  :ensure t
  :load-path "themes"
  :defer t
  :init
  (load-theme 'spacemacs-dark t))


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
  (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup) ; isearch で skk のセットアップ
  (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup) ; isearch で skk のクリーンアップ
  (setq skk-isearch-start-mode 'latin); isearch で skk の初期状態

  :config
  (setq skk-egg-like-newline t);;non-nilにするとEnterでの確定時に改行しない
  ;; ▼モードで BS を押したときには確定しないで前候補を表示する
  (setq skk-delete-implies-kakutei nil)
  (setq skk-dcomp-activate t)

 )


(use-package skk-study
  :disabled nil
  :after ddskk)

;;wget https://raw.githubusercontent.com/skk-dev/ddskk/master/bayesian/skk-bayesian.el

(use-package skk-bayesian
  :disabled t
  :init
  (add-to-list 'load-path "~/.emacs.d/elisp/usr/")
  )

(use-package htmlize
  :ensure t)


(use-package undo-tree
  :ensure t
  :bind (("M-/" . undo-tree-undo))
  :init
  (undo-tree-mode)
  :config
  (global-undo-tree-mode t)  )

(use-package company
  :ensure t
  :bind (
         :map company-mode-map
              ("C-M-i" . 'company-complete)
         :map company-active-map
              ("C-n"   . 'company-select-next)
              ("C-p"   . 'company-select-previous)
              ("C-s"   . 'company-filter-candidates)
              ("C-i"   . 'company-complete-selection)
         :map company-search-map
              ("C-n"   . 'company-select-next)
              ("C-p"   . 'company-select-previous)
              )
  :config
  (global-company-mode 1)
  ;(custom-set-variables '(company-idle-delay nil))
  )

(autoload 'run-prolog   "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode  "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.swi$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))


(setq recentf-max-saved-items 2000) ;; 2000ファイルまで履歴保存する
(setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
(setq recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/" "/\\.emacs\\.d/elpa/"))
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(put 'narrow-to-region 'disabled nil)



;; SLIMEのロード
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-repl slime-fancy slime-banner)) )

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

(use-package undohist
  :ensure t
  :config
  (undohist-initialize)
  ;;; 永続化を無視するファイル名の正規表現
  (setq undohist-ignored-files
        '("/tmp/" "COMMIT_EDITMSG"))
  )

(use-package auto-save-buffers-enhanced
  :ensure t
  :config
  ;;; 1秒後に保存
  (setq auto-save-buffers-enhanced-interval 1)
  (auto-save-buffers-enhanced t)
;;; Wroteのメッセージを抑制
  (setq auto-save-buffers-enhanced-quiet-save-p t)  )


(use-package web-mode
  :ensure t
  :config
  ;; (setq web-mode-extra-snippets
  ;;       '(("php" . (("print" . "<?php do { ?>\n\n<?php } while (|); ?>")
  ;;                   ("debug" . "<?php error_log(__LINE__); ?>")))
  ;;         ))
  (setq web-mode-extra-snippets
        '(("php" . (("print" . "print(\"|\")")
                    ))))
  )

(add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"     . web-mode))

;;helm

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
     ("M-y" . helm-show-kill-ring)
     ("C-x b" . helm-mini)
     ("C-x C-f" . helm-find-files)  )
  :config
  (helm-autoresize-mode 1)
  (helm-mode 1)  )

(use-package helm-config
  :config (helm-mode 1))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


(use-package which-key
  :ensure t
  :config
;;; 3つの表示方法どれか1つ選ぶ
  (which-key-setup-side-window-bottom)    ;ミニバッファ
;; (which-key-setup-side-window-right)     ;右端
;; (which-key-setup-side-window-right-bottom) ;両方使う
  (which-key-mode 1)  )

;;yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )


(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (yas-minor-mode        . " Ys")
    (paredit-mode          . " Pe")
    (eldoc-mode            . "")
    (abbrev-mode           . "")
    (undo-tree-mode        . " Ut")
    (elisp-slime-nav-mode  . " EN")
    (helm-gtags-mode       . " HG")
    (flymake-mode          . " Fm")
    ;; Major modes
    (lisp-interaction-mode . "Lisp")
    (python-mode           . "Python")
    (ruby-mode             . "Ruby")
    (emacs-lisp-mode       . "Elsp")
    (markdown-mode         . "Mkd")))

;;mode lineの設定
(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  )


(use-package spaceline :ensure t
  :disabled t
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-config :ensure spaceline
  :disabled t
  :config
  (spaceline-helm-mode 1)
  (spaceline-spacemacs-theme))

(use-package spaceline-all-the-icons
  :disabled t
  :after spaceline
  :config (spaceline-all-the-icons-theme)  )

(use-package smart-mode-line
  :ensure t
  :init
  (column-number-mode t) ;; 列番号の表示
  (line-number-mode t) ;; 行番号の表示
  (defvar sml/no-confirm-load-theme t)
  (defvar sml/theme 'dark) ;; お好みで
  (defvar sml/shorten-directory -1) ;; directory pathはフルで表示されたいので
  (sml/setup)  )

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(when (equal system-type 'gnu/linux)
  (add-to-list 'load-path "~/opt/mu-1.0/mu4e/")
  (cond ((display-graphic-p)
         (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "IPAex\346\230\216\346\234\235"))  )
        (t 0)))

(when (equal system-type 'darwin)
  (add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e/")
  (add-to-list 'exec-path " /usr/local/Cellar/phantomjs/2.1.1/bin/phantomjs/")
  (setenv "PATH" (mapconcat 'identity exec-path ":"))
    ;; Set your installed path
  (setq migemo-dictionary "/usr/local/Cellar/cmigemo/HEAD-5c014a8/share/migemo/utf-8/migemo-dict")
  (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "YuKyokasho Yoko"))
  ;フォント一覧を出力するには
                                        ;(dolist (x (font-family-list)) (print x))
  (setq org-plantuml-jar-path   "/usr/local/opt/plantuml/bin/plantuml")
  )

  


(use-package migemo
  :disabled t
  :ensure t
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  ;; Set your installed path
  ;;(setq migemo-dictionary "/usr/local/Cellar/cmigemo/HEAD-5c014a8/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init)
  (helm-migemo-mode 1)  )


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-python-pycompile-executable (executable-find "python3"))
  (setq flycheck-python-flake8-executable (executable-find "flake8"))
  (setq flycheck-python-pylint-executable (executable-find "pylint"))
  )

;; (with-temp-buffer
;;   (url-insert-file-contents "https://raw.github.com/steckerhalter/ob-php/master/ob-php.el")
;;   (eval-buffer))
;; (require 'ob-php)

;; (add-to-list 'org-babel-load-languages '(php . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(use-package sudo-edit
  :ensure t)
(setq-default tab-width 4 indent-tabs-mode nil)

(add-hook 'dired-load-hook
          (load "dired-x")
          (global-set-key "\C-x\C-j" 'skk-mode))

;(cua-mode t) ; cua-modeをオン
;(setq cua-enable-cua-keys nil)

(use-package php-mode
  :ensure t )


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python3")
  (setq py-python-command "python3")

    (add-hook 'python-mode-hook
        (lambda ()
            (setq-default indent-tabs-mode t)
            (setq-default tab-width 4)
            (setq-default py-indent-tabs-mode t)
            ))
  )

;; gtags-modeのキーバインドを有効化する
(setq gtags-suggested-key-mapping t) ; 無効化する場合はコメントアウト
;; ファイル保存時に自動的にタグをアップデートする
(setq gtags-auto-update t) ; 無効化する場合はコメントアウト

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)
  )

(put 'set-goal-column 'disabled nil)
(use-package lsp-mode
  :commands lsp
  :init
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t)

(use-package company-lsp
  :commands company-lsp
  :ensure t)
(use-package dimmer
  :ensure t
  :init
  (dimmer-mode)
  )
(use-package graphviz-dot-mode
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp/picasm/")
(require 'picasm)

(use-package rust-mode
  :ensure t
  :config
  (autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))


(when (eq system-type 'darwin)
  (when (executable-find "/usr/local/opt/ccls/bin/ccls")
    (setq ccls-executable "/usr/local/opt/ccls/bin/ccls")
    )
  )

(use-package smartparens-config
  :ensure smartparens
  :init
  (smartparens-global-mode))
