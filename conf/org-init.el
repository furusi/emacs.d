;;; init-org.el --- org settings

;;; Commentary:

;;; Code:

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :bind (("\C-cc" . org-capture)
         ("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb)
         :map org-mode-map
         ("C-c C-\'" . org-insert-structure-template)
         ("C-c C-u" . outline-up-heading-latin))
  :config
    (defun outline-up-heading-latin ()
    (interactive)
    (outline-up-heading 1 nil)
    (when (bound-and-true-p skk-mode)
      (skk-latin-mode nil)))

    (when (eq system-type 'gnu/linux)
    (setq org-directory (expand-file-name "~/pCloudDrive/org/")))
  (when (eq system-type 'darwin)
    (setq org-directory (expand-file-name "~/GoogleDrive/org/")))

  (when (not (file-exists-p org-directory))
      (setq org-directory (expand-file-name "~/org/"))
      (make-directory (concat org-directory "mobile/") t))

  (when (file-exists-p org-directory)
          (setq org-mobile-directory (concat org-directory "mobile/")))


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
          ("notes.org"        . (:level . 1))))
  (setq org-mobile-files
        (list
         (concat org-directory "task.org")
         (concat org-directory "notes.org")
         (concat org-directory "iphone.org")
         (concat org-directory "event.org")))
  (setq org-mobile-inbox-for-pull (concat org-directory "iphone.org"))
  (setq org-tag-alist
  '(("@OFFICE" . ?o) ("@HOME" . ?h) ("SHOPPING" . ?s)
    ("MAIL" . ?m) ("PROJECT" . ?p) ("備忘録" . ?b)))
  (setq org-capture-templates
        `(
          ("i" "インボックス" entry
           (file ,(concat org-directory "inbox.org"))
           "* %? %i\n %U\n")
          ("h" "定期的にやること" entry
           (file ,(concat org-directory "habit.org"))
           "* %?\n %U\n")
          ("t" "タスク" entry
           (file ,(concat org-directory "task.org"))
           "* TODO %? %i\n %a\n %U\n")
          ("e" "イベント" entry
           (file ,(concat org-directory "event.org"))
           "* EVENT %? %i\n %a\n %U\n")
          ("n"
           "ノート(本文から書く)"
           entry
           (file+headline, (concat org-directory "notes.org") "MEMO")
           "* %U \n\n%?\n%a\n%F\n")
          ("N"
           "ノート(見出しから書く)"
           entry
           (file+headline, (concat org-directory "notes.org") "MEMO")
           "* %U %?\n\n\n%a\n%F\n")
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
           "* %U %? %^g\n\n%a\n%F\n"
           :empty-lines 1)))
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
   (add-to-list 'org-babel-load-languages '(org . t))
   (add-to-list 'org-babel-load-languages '(C . t))
   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
   (add-hook 'org-mode-hook #'my-org-mode-hook)
   ;;ob-plantuml
   (add-to-list 'org-babel-default-header-args:plantuml '(:cmdline . "-charset utf-8")))


(use-package org-mobile-sync
  :ensure t
  :after (org)
  :config
  (org-mobile-sync-mode 1))
(use-package org-ac
  :ensure t
  :defer t
  :config
  (org-ac/config-default))
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

;;
;; Org mode
;;
;; Org Mode LaTeX Export

(use-package ox-bibtex
  :ensure org-plus-contrib
  :defer t)

(use-package ox-eldoc
  :ensure org-plus-contrib
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
  :ensure org-plus-contrib
  :after (org)
  :config
  (setq org-latex-default-class "bxjsarticle")
  ;; (setq org-latex-pdf-process '("latexmk -gg -pdfdvi  %f"))
  ;; (setq org-latex-pdf-process '("latexmk %f"))
  (setq org-latex-pdf-process '("latexmk -gg -pdfxe  %f"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-highlight-latex-and-related '(latex script entities))
;(setq org-latex-pdf-process '("latexmk -e '$lualatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdflua %f"))
                                        ;(setq org-export-in-background t)
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
\\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage{fancyhdr}
\\usepackage{listings, jlisting}
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
               "\\documentclass[unicode,12pt]{beamer}
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

(use-package ox-reveal
  :ensure t
  :disabled t)
(use-package ox-extra
  :ensure org-plus-contrib
  :after (org)
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))
(use-package ob-kotlin
  :after (org)
  :ensure t)
(use-package ob-rust
  :after (org)
  :ensure t)

(provide 'org-init)
;;; org-init.el ends here
