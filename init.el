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
    (package-install 'use-package))
    )

(show-paren-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/task.org" "~/Dropbox/org/notes.org" "~/Dropbox/org/habit.org" "~/Dropbox/org/event.org" "~/Dropbox/org/inbox.org")))
 '(org-src-lang-modes
   (quote
    (("html" . web)
     ("browser" . web)
     ("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("dot" . fundamental)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)
     ("shell" . sh)
     ("bash" . sh))))
 '(package-selected-packages
   (quote
    (howm helm-org-rifle company-irony irony company-php php-mode ssh-config-mode osx-dictionary plantuml-mode sudo-edit elisp-lint flycheck company-web common-lisp-snippets slime-company ob-browser ox-reveal migemo init-loader keyfreq esup spaceline-all-the-icons org-plus-contrib elpy exec-path-from-shell jedi yasnippet-snippets yasnippet which-key helm-themes leuven-theme highlight smartparens parent-mode highlight-parentheses helm web-mode auto-save-buffers-enhanced undohist fuzzy slime prodigy ox-rst sphinx-mode org-ac undo-tree atom-dark-theme gradle-mode package-utils magit manrkdown-mode ddskk))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package gradle-mode
  :ensure t)

(set-face-attribute 'default nil :height 140)



;; Emacs起動時にrst.elを読み込み
(use-package rst
  :ensure t
  :bind (
         :map rst-mode-map
         ("M-RET" . rst-insert-list))
  )
;; 拡張子の*.rst, *.restのファイルをrst-modeで開く
(setq auto-mode-alist
      (append '(( "\\.rst$"	 . rst-mode)
				( "\\.rest$" . rst-mode)
                ("\\.php$"   . web-mode)) auto-mode-alist))
;; 背景が黒い場合はこうしないと見出しが見づらい
;;(setq frame-background-mode 'dark)
;; 全部スペースでインデントしましょう
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  )

(use-package gradle-mode)

(use-package eww
  :config
  (setq eww-search-prefix "http://www.google.co.jp/search?q=")
  )

;;(setq coding-system-for-read 'utf-8)
;;(load-theme 'atom-dark t)
(load-theme 'leuven t)

(use-package ddskk
  :ensure t
;;  :defer t
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

;;
;; Org mode
;;
;; Org Mode LaTeX Export

(use-package ox-bibtex
  :ensure org-plus-contrib
  :defer t
  )
(use-package ox-latex
  :ensure org-plus-contrib
  :config
  (setq org-latex-default-class "bxjsarticle")
  (setq org-latex-pdf-process '("latexmk -gg -pdfdvi  %f"))
;(setq org-latex-pdf-process '("latexmk -e '$lualatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdflua %f"))
;(setq org-export-in-background t)
  (setq org-file-apps
        '(("pdf" . "open -a Skim %s")))

  (add-to-list 'org-latex-classes
               '("bxjsarticle"
                 "\\documentclass[twocolumn,autodetect-engine,dvi=dvipdfmx,10pt,a4paper,ja=standard]{bxjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\ifdefined\\kanjiskip
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
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
                 "\\documentclass[dvipdfmx,cjk]{beamer}
\\usepackage{bxdpx-beamer}
\\usepackage{pxjahyper}
\\usepackage{minijs}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}
\\newcommand{\\uline}[1]{\\underline{#1}}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  (add-to-list 'org-latex-classes
               '("ieicej"
                 
                 "\\documentclass[paper]{ieicej}
\\usepackage{graphicx}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{textcomp}
\\usepackage{latexsym}

\\setcounter{page}{1}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}"       . "\\section*\{%s\}")
                 ("\\subsection\{%s\}"    . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")
                 ("\\paragraph\{%s\}" . "\\paragraph*\{%s\}")
                 ("\\subparagraph\{%s\}" . "\\subparagraph*\{%s\}")))
  (setq org-latex-with-hyperref nil) ;ieicej出力時エラー対策

  )



;; org-export-latex-no-toc
(defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))
  (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

;; reftex with org mode
(add-hook 'org-mode-hook 'turn-on-reftex)
(defun org-mode-reftex-setup ()
   (load-library "reftex")
   (and (buffer-file-name)
        (file-exists-p (buffer-file-name))
        (reftex-parse-all))
   (define-key org-mode-map (kbd "C-c [") 'reftex-citation))



(use-package org-capture
  :requires org
  :bind (("\C-cc" . org-capture))
  :config
  (setq org-capture-templates
	`(
	  ("i" "インボックス" entry
	   (file ,(concat org-directory "inbox.org"))
	   "* %? %i\n %U\n")
	  ("t" "タスク" entry
	   (file ,(concat org-directory "task.org"))
	   "* TODO %? %i\n %U\n")
	  ("e" "イベント" entry
	   (file ,(concat org-directory "event.org"))
	   "* EVENT %? %i\n %U\n")
	  ("n" "ノート" entry
	   (file ,(concat org-directory "notes.org"))
	   "* %?\n %U\n")
	  ("h" "定期的にやること" entry
	   (file ,(concat org-directory "habit.org"))
	   "* %?\n %U\n")
	  ("T" "タスク(リンク付き)" entry
	   (file ,(concat org-directory "task.org"))
	   "* TODO %? %i\n %a\n %U\n")
	  ("E" "イベント(リンク付き)" entry
	   (file ,(concat org-directory "event.org"))
	   "* EVENT %? %i\n %a\n %U\n")
	  ("N" "ノート(リンク付き)" entry
	   (file ,(concat org-directory "notes.org"))
	   "* %?\n %a\n %U\n")
	  ("r" "読みかけ(リンク付き)" entry
	   (file ,(concat org-directory "reading.org"))
	   "* %?\n %a\n %U\n") ))
  )

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :init
  (setq org-directory (expand-file-name "~/Dropbox/org/"))
  :config
  (setq org-mobile-directory "~/Dropbox/アプリ/MobileOrg")
  (setq org-agenda-files
        '(    "~/Dropbox/org/task.org"
              "~/Dropbox/org/notes.org"
              "~/Dropbox/org/habit.org"
              "~/Dropbox/org/event.org"
              "~/Dropbox/org/inbox.org"
              "~/Dropbox/org/org-ical.org"))
  (setq org-refile-targets
        '(("org-ical.org" . (:level . 1))
          ("task.org"     . (:level . 1))
          ("event.org"    . (:level . 1))
          ("notes.org"    . (:level . 1))))
  (setq org-mobile-files
	    (list "~/Dropbox/org/notes.org"
	          "~/Dropbox/org/todo.org"
	          "~/Dropbox/org/task.org"
	          "~/Dropbox/org/iphone.org"
              "~/Dropbox/org/event.org"))
  (setq org-mobile-inbox-for-pull "~/Dropbox/org/iphone.org")
  (setq org-tag-alist
  '(("@OFFICE" . ?o) ("@HOME" . ?h) ("SHOPPING" . ?s)
    ("MAIL" . ?m) ("PROJECT" . ?p) ("備忘録" . ?b)))
  (setq org-refile-targets
	(quote (
		(nil . (:level . 1))
		(org-agenda-files . (:level . 1)))))
  ;; コードを評価するとき尋ねない
  (setq org-confirm-babel-evaluate nil)
  ;; 有効にする言語 デフォルトでは elisp のみ
  (org-babel-do-load-languages
   'org-babel-load-languages   '(
                                 (python   . t)
                                 (ruby     . t)
                                 (plantuml . t)
                                 (java     . t)
                                 (perl     . t)
                                 (dot      . t)))
   (setq org-refile-targets
       (quote (("notes.org" :level . 1)
               ("todo.org"  :level . 1)
               ("event.org" :level . 1)
               ("task.org"  :level . 1))))
   (setq org-use-speed-commands t)
   (setq org-icalendar-alarm-time 30)
   (setq org-icalendar-timezone "Asia/Tokyo")
   (defun my-org-mode-hook ()
     (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
   (add-hook 'org-mode-hook #'my-org-mode-hook)


  :bind (("\C-cl" . org-store-link)
	 ("\C-ca" . org-agenda)
	 ("\C-cb" . org-iswitchb)))
(use-package org-mobile-sync
  :ensure t
  :config
  (org-mobile-sync-mode 1)  )
(use-package htmlize
  :ensure t)


(use-package undo-tree
  :ensure t
  :bind (("M-/" . undo-tree-undo))
  :init
  (undo-tree-mode)
  :config
  (global-undo-tree-mode t)  )



(use-package org-ac
  :ensure t
  :defer t
  :config
  (org-ac/config-default)  )

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "org-ac")




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
  :ensure t)
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
  :defer t
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
    (python-mode           . "Py")
    (ruby-mode             . "Rb")
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

(use-package jedi
  :ensure t
  :defer t
  )
;; (add-hook 'python-mode-hook
;;           '(lambda()
;;              (jedi:ac-setup)
;;              (setq jedi:complete-on-dot t)
;;              (local-set-key (kbd "M-TAB") 'jedi:complete)))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))


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
  (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "IPAex\346\230\216\346\234\235"))  )
(when (equal system-type 'darwin)
  (add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e/")
  (add-to-list 'exec-path " /usr/local/Cellar/phantomjs/2.1.1/bin/phantomjs/")
  (setenv "PATH" (mapconcat 'identity exec-path ":"))
    ;; Set your installed path
  (setq migemo-dictionary "/usr/local/Cellar/cmigemo/HEAD-5c014a8/share/migemo/utf-8/migemo-dict")
  (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "Hiragino Mincho ProN"))
  ;フォント一覧を出力するには
                                        ;(dolist (x (font-family-list)) (print x))
  (setq org-plantuml-jar-path   "/usr/local/Cellar/plantuml/1.2018.9/libexec/plantuml.jar")
  )

  (use-package mu4e
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
    (setq
     user-mail-address "yourname@aaa.bbb"
     user-full-name  "yourname"
     mu4e-compose-signature
     (concat
      "aaaa\n"
      "xxxxxx\n") )
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
	       "football\\|soccer"			    (mu4e-message-field msg :subject))
	      "/football")
	     ;; messages sent by me go to the sent folder
	     ;;((find-if
	     ;;  (lambda (addr)
	     ;;	 (mu4e-message-contact-field-matches msg :from addr))
	     ;;     mu4e-user-mail-address-list)
	     ;;  mu4e-sent-folder)
	     ;; everything else goes to /archive
	     ;; important to have a catch-all at the end!
	     (t  "/archive"))	    )	  )
    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)
    ;; save attachment to my desktop (this can also be a function)
    (setq mu4e-attachment-dir "~/Downloads")
    (setq mu4e-maildir-shortcuts
	  '( ("/inbox"	 . ?i)	     
	     ("/sent"	 . ?s)
	     ("/trash"	 . ?t)
	     ("/archive" . ?a)))    )
  (use-package org-mu4e
    :config
    ;;store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil) )

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

(use-package ox-reveal
  :ensure t
  :config    )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-python-pycompile-executable "python3")
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

(use-package company-php
  :ensure t
  :init
    (add-hook 'php-mode-hook
          '(lambda ()
             (require 'company-php)
             (company-mode t)
             (ac-php-core-eldoc-setup) ;; enable eldoc
             (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-ac-php-backend))) )


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python3")
  (setq py-python-command "python3")
  )

;; gtags-modeのキーバインドを有効化する
(setq gtags-suggested-key-mapping t) ; 無効化する場合はコメントアウト
;; ファイル保存時に自動的にタグをアップデートする
(setq gtags-auto-update t) ; 無効化する場合はコメントアウト

(use-package irony
  :ensure t
  :config
  (progn  
    (use-package company-irony
      :ensure t
      :config      
      (add-to-list 'company-backends 'company-irony)
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))  ))

(use-package ox-extra
  :ensure org-plus-contrib
  :config
  (ox-extras-activate '(ignore-headlines)))

