;;; init.el --- my init script

;;; Commentary:

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(show-paren-mode t)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Language and Character Code
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-use-jump nil)
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
 '(easy-kill-alist
   (quote
    ((84 string-up-to-char-backward "")
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
     (98 buffer-file-name nil))))
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
 '(use-package-compute-statistics t)
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
(use-package initchart
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

(when (memq system-type '(darwin gnu/linux))
  (use-package exec-path-from-shell
    :config
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package system-packages
  :ensure t
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


(global-set-key (kbd "C-c t l") 'toggle-truncate-lines)
(global-set-key "\C-t" 'other-window)

;; C-u C-SPCの後C-SPCだけでマークを遡れる
(setq set-mark-command-repeat-pop t)
;; マークの数を32に増やす
(setq mark-ring-max 32)
(setq-default indent-tabs-mode nil)

(use-package restart-emacs
  :ensure t)


;; Emacs起動時にrst.elを読み込み
(use-package rst
  :ensure t
  :mode (("\\.rst$"  . rst-mode)
         ("\\.rest$" . rst-mode))
  :bind (:map rst-mode-map
              ("M-RET" . rst-insert-list))
  :config
  (when (eq system-type 'darwin)
    (setq rst-pdf-program "open -a Skim")
    (setq rst-slides-program "open -a Firefox")))

(use-package gradle-mode
  :mode (("\\.gradle$" . gradle-mode)))

(use-package eww
  :commands (eww)
  :config
  (setq eww-search-prefix "http://www.google.co.jp/search?q=")
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

(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))


;; SLIMEのロード
(use-package slime
  :straight slime-company
  :ensure-system-package sbcl
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))

(use-package undohist
  :ensure t
  :config
  (undohist-initialize)
  ;;; 永続化を無視するファイル名の正規表現
  (setq undohist-ignored-files
        '("/tmp/" "COMMIT_EDITMSG")))

(use-package undo-tree
  :init
  (global-undo-tree-mode t))


(use-package auto-save-buffers-enhanced
  :config
  ;; 1秒後に保存
  (setq auto-save-buffers-enhanced-interval 1)
  (auto-save-buffers-enhanced t)
  ;; Wroteのメッセージを抑制
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  ;; tramp mode時の自動保存を抑制
  (setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "/sudo:" "/multi:")))


(use-package web-mode
  :ensure t
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

(use-package all-the-icons  :ensure t)

(use-package which-key
  :ensure t
  :config
  ;; 3つの表示方法どれか1つ選ぶ
  (which-key-setup-side-window-bottom)    ;ミニバッファ
  ;; (which-key-setup-side-window-right)     ;右端
  ;; (which-key-setup-side-window-right-bottom) ;両方使う
  (which-key-mode 1))

;;;yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)


(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; ddskk
(use-package ddskk
  :straight (ddskk :type git :host github :repo "skk-dev/ddskk")
  :commands skk-mode
  :bind (("C-x C-j" . skk-mode))
  :hook (skk-mode . (lambda () (require 'context-skk))) ;自動的に英字モードになる
  :init
  (setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
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
  (add-hook 'dired-load-hook
            (load "dired-x")
            (global-set-key "\C-x\C-j" 'skk-mode))
  (setq skk-egg-like-newline t);;non-nilにするとEnterでの確定時に改行しない
  ;; ▼モードで BS を押したときには確定しないで前候補を表示する
  (setq skk-delete-implies-kakutei nil)
  (require 'skk-study)
  ;; ▼モード中で=漢字の読み方を指定する
  (setq skk-hint-start-char ?=)
  (require 'skk-hint))

;; Org-mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :straight org-plus-contrib
  :bind (("\C-cc" . org-capture)
         ("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb)
         :map org-mode-map
         ("C-c C-\'" . org-insert-structure-template)
         ("C-c C-u" . outline-up-heading-latin))
  :config
  (when window-system 'ns
        ;; org-modeのtableのフォントを設定
        (set-face-attribute 'org-table nil
                            :family "IPAGothic")
        (set-face-attribute 'org-formula nil
                            :family "IPAGothic"))
  (when (eq window-system 'x)
    (set-face-attribute 'org-table nil
                        :family "IPAゴシック")
    (set-face-attribute 'org-formula nil
                        :family "IPAゴシック")

    (add-to-list 'face-font-rescale-alist
                 '(".*IPAゴシック.*" . 0.85)))

  (defun outline-up-heading-latin ()
    (interactive)
    (outline-up-heading 1 nil)
    (when (bound-and-true-p skk-mode)
      (skk-latin-mode nil)))

  (when (equal system-type 'darwin)
    (setq org-plantuml-jar-path
          "/usr/local/opt/plantuml/libexec/plantuml.jar"))


  (when (eq system-type 'gnu/linux)
    (setq org-directory (expand-file-name "~/pCloudDrive/org/")))
  (when (eq system-type 'darwin)
    (setq org-directory (expand-file-name "~/Dropbox/org/")))

  (when (not (file-exists-p org-directory))
    (setq org-directory (expand-file-name "~/org/"))
    (make-directory (concat org-directory "mobile/") t))

  (setq org-agenda-files
        (list
         (concat org-directory "task.org")
         (concat org-directory "notes.org")
         (concat org-directory "habit.org")
         (concat org-directory "event.org")
         (concat org-directory "inbox.org")
         (concat org-directory "productivity.org")
         (concat org-directory "org-ical.org")))
  (setq org-refile-targets
        `(("org-ical.org"     . (:level . 1))
          ("task.org"         . (:level . 1))
          ("event.org"        . (:level . 1))
          ("productivity.org" . (:maxlevel . 2))
          ("notes.org"        . (:level . 2))))
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
           "* EVENT %? %i\n %a\n %U\n")
          ("n"
           "ノート(本文から書く)"
           entry
           (file+headline, (concat org-directory "notes.org") "MEMO")
           "* %U \n\n%?\n")
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
           (file+datetree (concat org-directory "minutes.org"))
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
  ;; コードを評価するとき尋ねない
  (setq org-confirm-babel-evaluate nil)

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



  (defun my-org-mode-hook ()
    (add-hook 'completion-at-point-functions
              'pcomplete-completions-at-point nil t))
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

(use-package org-mobile-sync
  :disabled t
  :after (org)
  :config
  (org-mobile-sync-mode 1)
  (when (file-exists-p org-directory)
    (setq org-mobile-directory (concat org-directory "mobile/"))
    (setq org-mobile-files
        (list
         (concat org-directory "task.org")
         (concat org-directory "notes.org")
         (concat org-directory "iphone.org")
         (concat org-directory "event.org")))
    (setq org-mobile-inbox-for-pull (concat org-directory "iphone.org"))))
(use-package org-mu4e
  :disabled t
  :load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e"
  :after (org)
  :config
  ;;store link to message if in header view, not to header query
  (setq org-mu4e-link-query-in-headers-mode nil))

(use-package org-journal
  :ensure t
  :after org
  :custom
  (org-journal-dir (concat org-directory "journal"))
  (org-journal-date-format "%A, %d %B %Y"))

(use-package ox-rst
  :after (org)
  :ensure t)
(use-package ox-hugo
  :ensure t
  :after org)
(use-package ob-browser
  :after org)
(use-package ox-epub
  :after org)

;; Org Mode LaTeX Export

(use-package ox-bibtex
  :straight nil
  :defer t)
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


(use-package ox-latex
  :straight nil
  :after (org)
  :config
  (setq org-latex-default-class "bxjsarticle")
  ;; (setq org-latex-pdf-process '("latexmk -gg -pdfdvi  %f"))
  ;; (setq org-latex-pdf-process '("latexmk %f"))
  (setq org-latex-pdf-process '("latexmk -gg -pdfxe  %f"))
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
\\setCJKmainfont{HiraginoSans-W4}
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
\\setCJKmainfont{BIZ-UDGothic}
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
  :after (org)
  :ensure t)
(use-package ob-rust
  :after (org)
  :ensure t)
(use-package ox-asciidoc
  :after (org)
  :ensure t)
(use-package ox-hugo
  :ensure t
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

(use-package ox-pandoc
  :ensure t
  :ensure-system-package pandoc
  :after ox)
(use-package org-download
  :ensure t
  :after org
  :hook ((org-mode . org-download-enable)))
(use-package org-seek
  :commands (org-seek-string org-seek-regexp org-seek-headlines)
  :ensure-system-package (rg . ripgrep)
  :config
  (setq org-seek-search-tool 'ripgrep))




(when (equal system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e/")
  (when window-system 'ns
        ;; 游教科書体
        ;; (set-face-attribute 'default nil
        ;;                     :family "YuKyokasho Yoko")
        ;; 源ノ角ゴシック
        (set-face-attribute 'default nil
                            :family "Source Han Code JP")
        ))
(when (equal system-type 'gnu/linux)
  (add-to-list 'load-path "~/opt/mu-1.0/mu4e/")
  ;;曖昧な文字幅を指定する
  (aset char-width-table ?→ 2)

  (when (eq window-system 'x)
    (set-face-attribute 'default nil
                        :family "源ノ角ゴシック Code JP")))

;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
(setq use-default-font-for-symbols nil)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk 'all))

(use-package mu4e
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


(use-package migemo
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
;;helm
(use-package helm-config
  :straight helm
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (helm-migemo-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
  (global-set-key (kbd "C-x b")   'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-s o")   'helm-occur)
  (global-set-key (kbd "C-x j")   'helm-recentf)
  (global-set-key (kbd "C-x r l") 'helm-bookmarks)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )

(use-package helm-swoop)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package helm-rg
  :ensure t
  :ensure-system-package (rg . ripgrep))
(use-package ace-jump-mode)
(use-package ace-isearch
  :after (ace-jump-mode helm-swoop)
  :config
  (global-ace-isearch-mode +1))

(use-package sudo-edit
  :ensure t)


(use-package company
  :ensure t
  :bind (
         :map company-mode-map
              ("C-M-i" . 'company-indent-or-complete-common)
         :map company-active-map
              ("C-n"   . 'company-select-next)
              ("C-p"   . 'company-select-previous)
              ("C-s"   . 'company-filter-candidates)
              ("C-i"   . 'company-complete-selection)
         :map company-search-map
              ("C-n"   . 'company-select-next)
              ("C-p"   . 'company-select-previous))
  :hook ((emacs-lisp-mode   . company-mode)
         (c-mode            . company-mode)
         (shell-script-mode . company-mode)
         (sh-mode           . company-mode)
         (shell-mode        . company-mode)
         (org-mode          . company-mode)
         (lisp-mode         . company-mode)
         (racer-mode        . company-mode)
         (rust-mode         . company-mode))
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

(use-package yatex
  :ensure t
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

(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :ensure t )
(use-package ac-php
  :after php-mode)
(use-package company-php
  :after (:all company php-mode ac-php)
  :hook (php-mode . (lambda ()
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
  :hook ((cc-mode     . lsp-deferred)
         (rust-mode   . lsp-deferred)
         (python-mode . lsp-deferred)))

;; ;; optionally
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :after lsp-mode)

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates nil)
  (company-lsp-async t)
  (company-lsp-enable-recompletion t)
  (company-lsp-enable-snippet t)
  :after
  (:all lsp-mode lsp-ui company yasnippet)
  :config
  (push 'company-lsp company-backends))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package lsp-java
  :after lsp-mode
  :hook (java-mode . lsp-deferred))
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1))
(use-package dap-java
  :straight dap-mode
  :after (lsp-java))
(use-package lsp-java-boot
  :straight nil
  :hook ((lsp-mode . lsp-lens-mode)
         (java-mode . lsp-java-lens-mode)))

(use-package hydra
  :ensure t)
(use-package projectile
  :ensure t)


;; ;;git clone git@github.com:rswarbrick/picasm.git ~/.emacs.d/lisp/picasm
;; (use-package picasm
;;   :load-path "~/.emacs.d/lisp/picasm/")

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode))
  :config
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

(use-package android-mode)

(use-package ccls
  :commands ccls
  :ensure-system-package ccls
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



(use-package smartparens-config
  :straight smartparens
  :init
  (smartparens-global-mode))

(use-package kotlin-mode
  :ensure t
  :mode (("\\.kt\\'" . kotlin-mode)))

(use-package whitespace
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


(use-package plantuml-mode
  :ensure t
  :ensure-system-package plantuml
  :config
  (when (eq system-type 'darwin)
    (setq plantuml-jar-path
          "/usr/local/opt/plantuml/libexec/plantuml.jar")))

(use-package htmlize
  :ensure t)
(use-package adoc-mode
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
(use-package npm-mode
  :ensure t
  :ensure-system-package npm)
(use-package autodisass-java-bytecode
  :defer t)

(use-package google-c-style
  :defer t
  :commands
  (google-set-c-style))
(use-package regex-tool)

(use-package easy-kill
  :commands (easy-kill easy-mark)
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))
(use-package easy-kill-extras)

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
