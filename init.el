;;; init.el --- my init script

;;; Commentary:

;;; Code:


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
    (package-install 'use-package)))

(show-paren-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.ehist"))))
 '(company-global-modes
   (quote
    (not org-mode magit-mode custom-mode magit-status-mode magit-revision-mode magit-diff-mode)))
 '(company-idle-delay 0.2)
 '(company-lsp-cache-candidates (quote auto))
 '(dimmer-exclusion-regexp "^\\\\*helm\\\\|^ \\\\*Minibuf\\\\|^\\\\*Calendar\"")
 '(dimmer-fraction 0.3)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eval-expression-print-length nil)
 '(global-company-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-display-buffer-function (quote magit-display-buffer-fullframe-status-v1))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (C . t) (dot . t))))
 '(org-export-backends (quote (ascii html icalendar latex odt taskjuggler)))
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-dir "/Users/furusho/GoogleDrive/org/journal")
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
     ("asm" . picasm))))
 '(org-src-preserve-indentation t)
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
    (back-button python-mode jedi jedi-core lsp-java-treemacs dap-java flycheck-rust cargo racer howm counsel-tramp dropbox editorconfig editorconfig-generate ox-pandoc c-eldoc ggtags graphviz-dot-mode kotlin-mode php-mode visual-regexp-steroids omnisharp dap-mode treemacs lsp-java ccls zenburn-theme yatex yasnippet-snippets which-key web-mode use-package undohist undo-tree sudo-edit spacemacs-theme smartparens smart-mode-line slime rust-mode restart-emacs poet-theme plantuml-mode pipenv ox-rst ox-reveal org-plus-contrib org-mobile-sync org-journal org-ac nim-mode magit-popup magit lsp-ui keyfreq htmlize helm gradle-mode exec-path-from-shell elpy dimmer ddskk company-web company-shell company-php company-lsp company-jedi company-irony auto-save-buffers-enhanced)))
 '(picasm-db-file "~/.emacs.d/lisp/picasm/picasm-db.el")
 '(plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude
   (quote
    ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" "/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/" "/\\.emacs\\.d/elpa/")))
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 2000)
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

(recentf-mode 1)

(use-package use-package-ensure-system-package
  :ensure t)
(add-to-list 'load-path "~/.emacs.d/conf")
(load "org-init")
(load "yatex-init")
(load "mu4e-init")
(load "magit-init")
(load "company-init")
(define-key global-map (kbd "C-c t l") 'toggle-truncate-lines)
(add-hook 'dired-load-hook
          (load "dired-x")
          (global-set-key "\C-x\C-j" 'skk-mode))
(global-set-key "\C-t" 'other-window)

(setq-default indent-tabs-mode nil)

;; eww
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
(add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)
;; eww

(use-package restart-emacs
  :ensure t)

(use-package gradle-mode
  :ensure t)





;; Emacs起動時にrst.elを読み込み
(use-package rst
  :ensure t
  :bind (
         :map rst-mode-map
              ("M-RET" . rst-insert-list))
  :config
  (when (eq system-type 'darwin)
    (setq rst-pdf-program "open -a Skim")
    (setq rst-slides-program "open -a Firefox")))

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
  (setq eww-search-prefix "http://www.google.co.jp/search?q="))

;;(setq coding-system-for-read 'utf-8)
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
  (setq skk-dcomp-activate t))


(use-package skk-study
  :disabled nil
  :after ddskk)

;;wget https://raw.githubusercontent.com/skk-dev/ddskk/master/bayesian/skk-bayesian.el

(use-package skk-bayesian
  :disabled t
  :init
  (add-to-list 'load-path "~/.emacs.d/elisp/usr/"))

(use-package htmlize
  :ensure t)


(use-package undo-tree
  :ensure t
  :bind (("M-/" . undo-tree-undo))
  :init
  (undo-tree-mode)
  :config
  (global-undo-tree-mode t))


(autoload 'run-prolog   "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode  "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.swi$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))


(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(put 'narrow-to-region 'disabled nil)



;; SLIMEのロード
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-repl slime-fancy slime-banner)))

(use-package undohist
  :ensure t
  :config
  (undohist-initialize)
  ;;; 永続化を無視するファイル名の正規表現
  (setq undohist-ignored-files
        '("/tmp/" "COMMIT_EDITMSG")))

(use-package auto-save-buffers-enhanced
  :ensure t
  :config
  ;;; 1秒後に保存
  (setq auto-save-buffers-enhanced-interval 1)
  (auto-save-buffers-enhanced t)
;;; Wroteのメッセージを抑制
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  ;;; tramp mode時の自動保存を抑制
  (setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "/sudo:" "/multi:")))


(use-package web-mode
  :ensure t
  :config
  ;; (setq web-mode-extra-snippets
  ;;       '(("php" . (("print" . "<?php do { ?>\n\n<?php } while (|); ?>")
  ;;                   ("debug" . "<?php error_log(__LINE__); ?>")))
  ;;         ))
  (setq web-mode-extra-snippets
        '(("php" . (("print" . "print(\"|\")"))))))

(add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"     . web-mode))

;;helm
(when (eq window-system 'ns)
  (use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
     ("M-y" . helm-show-kill-ring)
     ("C-x b" . helm-mini)
     ("C-x C-f" . helm-find-files)
     ("C-c C-s" . helm-occur)
     ("C-x j" . helm-recentf))
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (helm-autoresize-mode 1)
  (helm-mode 1))
(use-package helm-config
  :config (helm-mode 1))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package all-the-icons
  :ensure t))

;; ivy/counsel
(when (eq window-system nil)
  (use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x j" . counsel-recentf)
         ("C-c C-i" . counsel-imenux)
         ("C-s"   . swiper-isearch)
         ("M-y"   . counsel-yank-pop))
  :init
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil))
(use-package counsel-tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh"))
(use-package flx
  :ensure t)
(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setqup)))



;; ido
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (setq ido-enable-flex-matching t) ;; 中間/あいまい一致



(use-package which-key
  :ensure t
  :config
;;; 3つの表示方法どれか1つ選ぶ
  (which-key-setup-side-window-bottom)    ;ミニバッファ
;; (which-key-setup-side-window-right)     ;右端
;; (which-key-setup-side-window-right-bottom) ;両方使う
  (which-key-mode 1))

;;yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)


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
    (editorconfig-mode     . "")
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
  (exec-path-from-shell-initialize))


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
  :config (spaceline-all-the-icons-theme))

(use-package smart-mode-line
  :ensure t
  :init
  (column-number-mode t) ;; 列番号の表示
  (line-number-mode t) ;; 行番号の表示
  (defvar sml/no-confirm-load-theme t)
  (defvar sml/theme 'dark) ;; お好みで
  (defvar sml/shorten-directory -1) ;; directory pathはフルで表示されたいので
  (sml/setup))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

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
         (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "IPAex\346\230\216\346\234\235")))
        (t 0))
  (add-to-list 'face-font-rescale-alist
               '(".*IPAex.*" . 1.1))
  (setq org-plantuml-jar-path   "/usr/bin/plantuml"))

(when (equal system-type 'darwin)
  (add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e/")
  (add-to-list 'exec-path " /usr/local/Cellar/phantomjs/2.1.1/bin/phantomjs/")
  (setenv "PATH" (mapconcat 'identity exec-path ":"))
  ;; Set your installed path
  (setq migemo-dictionary "/usr/local/Cellar/cmigemo/HEAD-5c014a8/share/migemo/utf-8/migemo-dict")
  (cond ((display-graphic-p)


         ;;Osaka
         ;; (set-frame-font "-*-Osaka-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" 13)


         ;; (add-to-list 'face-font-rescale-alist
         ;;              '(".*Source\ Han\ Code\ JP\ N.*" . 1.3))


         ;; 源ノ角ゴシック
         ;; (set-default-font "Source Han Code JP N")
         ;; (add-to-list 'face-font-rescale-alist
         ;;              '(".*Source\ Han\ Code\ JP\ N.*" . 1.3))

         ;; 游教科書体
         (set-face-attribute 'default nil :height 130)
         (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "YuKyokasho Yoko"))
         (add-to-list 'face-font-rescale-alist
                      '(".*YuKyokasho.*" . 1.3))

         ;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
         (setq use-default-font-for-symbols nil))
        (t 0))
  (setq org-plantuml-jar-path   "/usr/local/opt/plantuml/libexec/plantuml.jar"))


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
  (helm-migemo-mode 1))


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-python-pycompile-executable (executable-find "python3"))
  (setq flycheck-python-flake8-executable (executable-find "flake8"))
  (setq flycheck-python-pylint-executable (executable-find "pylint")))

;; (with-temp-buffer
;;   (url-insert-file-contents "https://raw.github.com/steckerhalter/ob-php/master/ob-php.el")
;;   (eval-buffer))
;; (require 'ob-php)

;; (add-to-list 'org-babel-load-languages '(php . t))


(use-package sudo-edit
  :ensure t)
(setq-default tab-width 4 indent-tabs-mode nil)

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
  ;; (push '(company-semantic :with company-yasnippet) company-backends)
  ;(custom-set-variables '(company-idle-delay nil))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-enable-recompletion t))


;(cua-mode t) ; cua-modeをオン
;(setq cua-enable-cua-keys nil)

(use-package php-mode
  :ensure t )


(use-package python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-default indent-tabs-mode t)
              (setq-default tab-width 4)
              (setq-default py-indent-tabs-mode t)))
  :config
  (setq python-shell-interpreter "python3")
  ;; (add-hook 'python-mode-hook #'lsp)
  (setq py-python-command "python3"))
(use-package jedi
  :ensure t)
(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (add-to-list 'company-backends 'company-jedi))


(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(put 'set-goal-column 'disabled nil)

;; gtags-modeのキーバインドを有効化する
(setq gtags-suggested-key-mapping t) ; 無効化する場合はコメントアウト
;; ファイル保存時に自動的にタグをアップデートする
(setq gtags-auto-update t) ; 無効化する場合はコメントアウト


(use-package lsp-mode
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package company-lsp
  :commands company-lsp
  :ensure t)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dimmer
  :ensure t
  :init
  (dimmer-mode)
  )
(use-package graphviz-dot-mode
  :ensure t)


;;git clone git@github.com:rswarbrick/picasm.git ~/.emacs.d/lisp/picasm
(use-package picasm
  :load-path "~/.emacs.d/lisp/picasm/")

(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq company-tooltip-align-annotations t))
(use-package racer
  :ensure t
  :hook ((rust-mode  . racer-mode)
         (racer-mode . eldoc-mode)))
(use-package cargo
  :ensure t)
(use-package flycheck-rust
  :ensure t
  :config
  (setq flycheck-rust-cargo-executable "~/.cargo/bin/cargo")
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (when (eq system-type 'darwin)
  (when (executable-find "/usr/local/opt/ccls/bin/ccls")
    (setq ccls-executable "/usr/local/opt/ccls/bin/ccls"))))



(use-package smartparens-config
  :ensure smartparens
  :init
  (smartparens-global-mode))

(use-package kotlin-mode
  :ensure t
  :mode (("\\.kt\\'" . kotlin-mode)))

(use-package projectile :ensure t)
(use-package treemacs :ensure t)
(use-package hydra :ensure t)
(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java :after (lsp-java))
(use-package lsp-java-treemacs :after (treemacs))
(require 'lsp-java-boot)

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(use-package whitespace
  ;; :disabled t
  :config
  ;; 空白
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

(use-package omnisharp
  :ensure t
  :hook (csharp-mode . omnisharp-mode)
  :init
  (add-to-list 'company-backends 'company-omnisharp))
(use-package visual-regexp-steroids
  :ensure t)

(use-package ox-pandoc
  :ensure t
  :after org-plus-contrib
  :demand t
  :config
  ;; default options for all output formats
(setq org-pandoc-options '((standalone . t)))
;; cancel above settings only for 'docx' format
(setq org-pandoc-options-for-docx '((standalone . nil))))
(use-package plantuml-mode
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")))
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package editorconfig-generate
  :ensure t)
(use-package howm
  :ensure t)
(use-package back-button
  :ensure t
  :config
  (back-button-mode 1))
(provide 'init)
;;; init.el ends here
