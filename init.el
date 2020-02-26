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
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(eval-when-compile
  (unless (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)))

(show-paren-mode t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-function (quote ace-jump-char-mode))
 '(ace-isearch-jump-delay 0.6)
 '(backup-directory-alist (quote ((".*" . "~/.ehist"))))
 '(comment-style (quote multi-line))
 '(company-global-modes
   (quote
    (not org-mode magit-mode custom-mode magit-status-mode magit-revision-mode magit-diff-mode)))
 '(company-idle-delay 0.2)
 '(company-lsp-cache-candidates (quote auto))
 '(custom-enabled-themes (quote (tango)))
 '(dimmer-exclusion-regexp "^\\\\*helm\\\\|^ \\\\*Minibuf\\\\|^\\\\*Calendar\"")
 '(dimmer-fraction 0.3)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eval-expression-print-length nil)
 '(global-company-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-display-buffer-function (quote magit-display-buffer-fullframe-status-v1))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (C . t) (dot . t))))
 '(org-export-backends (quote (ascii html icalendar latex md odt taskjuggler)))
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-dir "/Users/furusho/Dropbox/org/journal")
 '(org-latex-default-class "bxjsarticle")
 '(org-latex-listings t)
 '(org-latex-listings-options
   (quote
    (("frame" "single")
     ("basicstyle" "{\\ttfamily\\scriptsize}")
     ("numbers" "left")
     ("commentstyle" "{\\ttfamily\\scriptsize}")
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
     ("asm" . asm))))
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
    (easy-hugo lsp-mode use-package-ensure-system-package spinner company yasnippet all-the-icons ob-kotlin ace-jump-mode ace-isearch helm-swoop helm-migemo migemo gnu-elpa-keyring-update rustic review-mode pandoc ox-epub ob-browser htmlize adoc-mode ox-asciidoc ox-hugo org company-arduino arduino-mode pandoc-mode lorem-ipsum undo-propose 0x0 all-the-icons-ivy groovy-mode ob-rust multi-term back-button jedi jedi-core lsp-java-treemacs dap-java flycheck-rust cargo racer howm counsel-tramp dropbox editorconfig editorconfig-generate ox-pandoc c-eldoc ggtags graphviz-dot-mode kotlin-mode php-mode visual-regexp-steroids omnisharp dap-mode treemacs lsp-java ccls zenburn-theme yatex yasnippet-snippets which-key web-mode use-package undohist undo-tree sudo-edit spacemacs-theme smartparens smart-mode-line slime rust-mode restart-emacs poet-theme plantuml-mode pipenv ox-rst ox-reveal org-plus-contrib org-mobile-sync org-journal org-ac nim-mode magit-popup magit lsp-ui keyfreq helm gradle-mode exec-path-from-shell elpy dimmer ddskk company-web company-shell company-php company-lsp company-jedi company-irony auto-save-buffers-enhanced)))
 '(php-manual-url (quote ja))
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
 '(skk-isearch-mode-string-alist
   (quote
    ((hiragana . "[か] ")
     (katakana . "[カ] ")
     (jisx0208-latin . "[英] ")
     (latin . "")
     (abbrev . "[aあ] ")
     (nil . "[--] "))))
 '(sp-escape-quotes-after-insert nil)
 '(zenburn-scale-org-headlines t)
 '(zenburn-scale-outline-headlines t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; (add-to-list 'load-path "~/.emacs.d/lisp/picasm")
;; (require 'initchart)
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)



(recentf-mode 1)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package system-packages
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (setq system-packages-use-sudo nil
          system-packages-package-manager 'brew))
  (when (eq system-type 'gnu/linux)
    (setq system-packages-use-sudo t
          system-packages-package-manager 'apt))
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

(add-to-list 'load-path "~/.emacs.d/conf")
(load "org-init")
(load "skk-init")
(load "helm-init")
(load "magit-init")
(load "yatex-init")
(load "mu4e-init")

(define-key global-map (kbd "C-c t l") 'toggle-truncate-lines)

(global-set-key "\C-t" 'other-window)
(defun which-linux-distribution ()
  "from lsb_release"
  (interactive)
  (when (eq system-type 'gnu/linux)
    (shell-command-to-string "lsb_release -sd")))

(setq-default indent-tabs-mode nil)

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

(use-package gradle-mode)

(use-package eww
  :commands (eww)
  :config
  (setq eww-search-prefix "http://www.google.co.jp/search?q=")
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
  (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image))



(autoload 'run-prolog   "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode  "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.swi$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))


(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))



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

(use-package undo-tree
  :ensure t
  :pin gnu
  :bind (("M-/" . undo-tree-undo))
  :init
  (global-undo-tree-mode t))


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

(use-package all-the-icons  :ensure t)



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


(add-to-list 'load-path "~/.nix-profile/bin")



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
  ;;曖昧な文字幅を指定する
  (aset char-width-table ?→ 2)

  (when (eq window-system 'x)
    (set-face-attribute 'default nil
                        :family "Source Code Pro")
    (set-fontset-font
     nil 'japanese-jisx0208
     (font-spec :family "源ノ角ゴシック Code JP"))
    (set-fontset-font
     nil '(#x2190 . #x21EF)
     (font-spec :family "源ノ角ゴシック Code JP"))

    (add-to-list 'face-font-rescale-alist
                 '(".*源ノ角ゴシック.*" . 1.2)))

  (setq org-plantuml-jar-path   "/usr/bin/plantuml"))

(use-package migemo
  :ensure t
  :ensure-system-package cmigemo
  :config
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-coding-system 'utf-8-unix)
  ;; Set your installed path
  (when (eq system-type 'darwin)
    (setq migemo-command "/usr/local/bin/cmigemo")
    (setq migemo-dictionary "/usr/local/opt/cmigemo/share/migemo/utf-8/migemo-dict"))
  (when (eq system-type 'gnu/linux)
    (setq migemo-command "/usr/bin/cmigemo")
    (if (string-match-p "arch" operating-system-release)
        (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
        (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init))
(use-package ace-jump-mode
  :ensure t)
(use-package ace-isearch
  :ensure t
  :config
  (global-ace-isearch-mode 1))
(use-package helm-swoop
  :ensure t)



(when (equal system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e/")
  ;; (add-to-list 'exec-path " /usr/local/Cellar/phantomjs/2.1.1/bin/phantomjs/")
  (setenv "PATH" (mapconcat 'identity exec-path ":"))
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
                      '(".*YuKyokasho.*" . 1.3)))
        (t 0))
  (setq org-plantuml-jar-path   "/usr/local/opt/plantuml/libexec/plantuml.jar"))

;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
(setq use-default-font-for-symbols nil)


(use-package sudo-edit
  :ensure t)


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
  :hook ((org-mode . company-mode)
         (racer-mode . company-mode)
         (rust-mode . company-mode))
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

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))


(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :ensure t )

(use-package lsp-mode
  :ensure t
  :hook ((java-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (python-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

;; ;; optionally
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; ;; optionally if you want to use debugger
;; (use-package lsp-java :ensure t :after lsp
;;   :config (add-hook 'java-mode-hook 'lsp))
;; (use-package dap-mode
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1))
;; (use-package dap-java :after (lsp-java))
;; (use-package hydra :ensure t)
;; (use-package projectile :ensure t)



;; ;;git clone git@github.com:rswarbrick/picasm.git ~/.emacs.d/lisp/picasm
;; (use-package picasm
;;   :load-path "~/.emacs.d/lisp/picasm/")

(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))
(use-package rustic
  :disabled
  :ensure t
  :config
  (setq rustic-lsp-server 'rust-analyzer))
(use-package racer
  :ensure t
  :hook ((rust-mode  . racer-mode)
         (racer-mode . eldoc-mode)))
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package ccls
  :ensure t
  :defer t
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


(use-package plantuml-mode
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")))
(use-package back-button
  :ensure t
  :config
  (back-button-mode 1))

(use-package htmlize
  :ensure t)
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)
(use-package adoc-mode
  :ensure t)
(use-package ob-browser
  :ensure t)
(use-package ox-epub
  :ensure t)
(use-package pandoc
  :ensure t)
(use-package graphviz-dot-mode
  :ensure t)
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package easy-hugo
  :ensure t
  :config
  (setq easy-hugo-org-header t)
  (setq easy-hugo-default-ext ".org"))

;; (use-package evil
;;   :ensure t
;;   :config
;;   (evil-mode 1))

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
(setq make-backup-files t)

;;;ediff時にorgファイルを全て表示する
(with-eval-after-load 'outline
  (add-hook 'ediff-prepare-buffer-hook #'org-show-all))

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

;;行番号を表示
(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      (setq-default indicate-empty-lines t)
      (setq-default indicate-buffer-boundaries 'left)))


(load-file "~/.emacs.d/lisp/window.el")

(provide 'init)
;;; init.el ends here
