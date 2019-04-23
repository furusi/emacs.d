;;; package --- Summary

;;; Commentary:

;;; Code:

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
 ;(setq tex-command "ptex2pdf -u -l -ot '-synctex=1'")
 ;(setq tex-command "lualatex -synctex=1")
 ;(setq tex-command "latexmk")
  (setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -shell-escape -norc -gg -pdfdvi")
 ;(setq tex-command "latexmk -e '$lualatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
  (setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
  (setq makeindex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
  (setq dvi2-command "open -a Skim")
 ;(setq dvi2-command "open -a Preview")
 ;(setq dvi2-command "open -a TeXShop")
 ;(setq dvi2-command "/Applications/TeXworks.app/Contents/MacOS/TeXworks")
 ;(setq dvi2-command "/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only")
  (setq tex-pdfview-command "open -a Skim")
 ;(setq tex-pdfview-command "open -a Preview")
 ;(setq tex-pdfview-command "open -a TeXShop")
 ;(setq tex-pdfview-command "/Applications/TeXworks.app/Contents/MacOS/TeXworks")
 ;(setq tex-pdfview-command "/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only")
  (setq dviprint-command-format "open -a \"Adobe Acrobat Reader DC\" `echo %s | gsed -e \"s/\\.[^.]*$/\\.pdf/\"`")
  (add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))
  (add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region))))
; for yatex
(when (equal system-type 'darwin)
  (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:/Applications/Skim.app/Contents/SharedSupport:$PATH" t)
  (setq exec-path (append '("/usr/local/bin" "/Library/TeX/texbin" "/Applications/Skim.app/Contents/SharedSupport") exec-path)))

(provide 'yatex-init)
;;; yatex-init.el ends here
