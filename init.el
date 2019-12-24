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
 '(org-export-backends (quote (ascii html icalendar latex md odt taskjuggler)))
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-dir "/home/furusho/pCloudDrive/org/journal")
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
    (htmlize adoc-mode ox-asciidoc ox-hugo org company-arduino arduino-mode pandoc-mode lorem-ipsum undo-propose 0x0 all-the-icons-ivy groovy-mode ob-rust multi-term back-button jedi jedi-core lsp-java-treemacs dap-java flycheck-rust cargo racer howm counsel-tramp dropbox editorconfig editorconfig-generate ox-pandoc c-eldoc ggtags graphviz-dot-mode kotlin-mode php-mode visual-regexp-steroids omnisharp dap-mode treemacs lsp-java ccls zenburn-theme yatex yasnippet-snippets which-key web-mode use-package undohist undo-tree sudo-edit spacemacs-theme smartparens smart-mode-line slime rust-mode restart-emacs poet-theme plantuml-mode pipenv ox-rst ox-reveal org-plus-contrib org-mobile-sync org-journal org-ac nim-mode magit-popup magit lsp-ui keyfreq helm gradle-mode exec-path-from-shell elpy dimmer ddskk company-web company-shell company-php company-lsp company-jedi company-irony auto-save-buffers-enhanced)))
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
(use-package use-package-ensure-system-package
  :ensure t)
(add-to-list 'load-path "~/.emacs.d/conf")
(load "org-init")
(load "yatex-init")
(load "mu4e-init")
(load "magit-init")
(load "skk-init")
(define-key global-map (kbd "C-c t l") 'toggle-truncate-lines)
(add-hook 'dired-load-hook
          (load "dired-x")
          (global-set-key "\C-x\C-j" 'skk-mode))
(global-set-key "\C-t" 'other-window)

;; (setq-default indent-tabs-mode nil)

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
  :bind (("M-/" . undo-tree-undo))
  :init
  (undo-tree-mode)
  :config
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

;;helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c C-s" . helm-occur)
         ("C-x j" . helm-recentf)
         ("C-x r l" . helm-bookmarks))
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (helm-autoresize-mode 1)
  (helm-mode 1))
(use-package helm-config
  :config (helm-mode 1))
(use-package helm-lsp  :commands helm-lsp-workspace-symbol)
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


(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))
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
  (cond ((display-graphic-p)
         (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "IPAex\346\230\216\346\234\235")))
        (t 0))
  (add-to-list 'face-font-rescale-alist
               '(".*IPAex.*" . 1.1))
  (setq org-plantuml-jar-path   "/usr/bin/plantuml"))

(when (equal system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e/")
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

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))


(use-package php-mode
  :mode (("\\.php\\'" . web-mode))
  :ensure t )

(use-package lsp-mode
  :hook (java-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

;; ;; optionally
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; ;; optionally if you want to use debugger
;; (use-package lsp-java :ensure t :after lsp
;;   :config (add-hook 'java-mode-hook 'lsp))
;; (use-package dap-mode
;;   :ensure t :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))
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
  (setq company-tooltip-align-annotations t))
(use-package racer
  :ensure t
  :hook ((rust-mode  . racer-mode)
         (racer-mode . eldoc-mode)))
(use-package cargo
  :ensure t)

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
  :disabled t ;;org-publishが動かない
  :ensure t)
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)
(use-package adoc-mode
  :ensure t)
(use-package htmlize
  :ensure t)
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

(provide 'init)
;;; init.el ends here
