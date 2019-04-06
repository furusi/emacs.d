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
         ("C-c C-\'" . org-insert-structure-template))
  :init
  (setq org-directory (expand-file-name "~/Dropbox/org/"))
  :config
  (setq org-mobile-directory "~/Dropbox/アプリ/MobileOrg")
  (setq org-agenda-files
        '("~/Dropbox/org/task.org"
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
           "思いついたことを書く"
           entry
           (file+headline, (concat org-directory "notes.org") "MEMO")
           "* %U%?\n\n%a\n%F\n"
           );; "* %?\n %a\n %U\n"
          ("r" "読みかけ(リンク付き)" entry
           (file ,(concat org-directory "reading.org"))
           "* %?\n %a\n %U\n")
          ("m"
           "みんなで会議"
           entry
           (file+datetree "~/Documents/org/minutes.org")
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
           (file+headline "~/Dropbox/org/gtd.org" "GTD")
           "** TODO %T %?\n   Entered on %U    %i\n"
           :empty-lines 1)
          ("i"
           "itemのテスト"
           item
           (file+headline "~/Dropbox/org/gtd.org" "GTD")
           "** TODO %T %?\n   Entered on %U    %i\n"
           :empty-lines 1)
          ("z"
           "'あれ'についてのメモ"
           entry
           (file+headline , (concat org-directory "notes.org") "MEMO")
           "* %U%?%^g\n\n%a\n%F\n"
           :empty-lines 1)
          
          ))
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
)


(use-package org-mobile-sync
  :ensure t
  :config
  (org-mobile-sync-mode 1)  )
(use-package org-ac
  :ensure t
  :defer t
  :config
  (org-ac/config-default)  )
  (use-package org-mu4e
    :config
    ;;store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil) )

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/Dropbox/org/journal")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package ox-rst
  :ensure t)

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
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-highlight-latex-and-related '(latex script entities))
;(setq org-latex-pdf-process '("latexmk -e '$lualatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdflua %f"))
                                        ;(setq org-export-in-background t)
  (when (equal system-type 'darwin)
    (setq org-file-apps
          '(("pdf" . "open -a Skim %s")))    )
  (when (equal system-type 'gnu/linux)
    (setq org-file-apps
          '(("pdf" . "evince %s")))    )

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
                 "\\documentclass[dvipdfmx,cjk]{beamer}
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
(use-package ox-reveal
  :ensure t
  :disabled t)
(use-package ox-extra
  :ensure org-plus-contrib
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))
