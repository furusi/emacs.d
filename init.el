;;; init.el --- my init script

;;; Commentary:

;;; Code:


(show-paren-mode t)
(column-number-mode)

;; 絵文字のフォント設定
(when window-system
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :config
  (load-file (locate-user-emacs-file "custom.el")))

(leaf custom-variables
  :doc "set custom variables"
  :custom
  `((auth-sources . '(,(expand-file-name "authinfo.gpg" user-emacs-directory)))
    (auto-save-interval . 10)
    (backup-directory-alist . '((".*" . "~/.ehist")))
    (byte-compile-warnings . '(cl-functions))
    (comment-style . 'multi-line)
    (custom-theme-directory . ,(concat user-emacs-directory "themes/")) ;; テーマのディレクトリを設定
    (default-frame-alist .'((width . 180) (height . 40)))
    (dired-dwim-target . t)
    (ediff-diff-options . "-w")
    (ediff-split-window-function . 'split-window-horizontally)
    (ediff-window-setup-function . 'ediff-setup-windows-plain)
    (indent-tabs-mode . nil)
    (inhibit-startup-screen . t)
    (mark-ring-max . 32);; マークの数を32に増やす
    (menu-bar-mode . t)
    (recentf-auto-cleanup . 'never)
    (recentf-max-menu-items . 30)
    (recentf-max-saved-items . 2000)
    (safe-local-variable-values . '((org-export-directory . "~/Dropbox/org")))
    (set-mark-command-repeat-pop . t)    ;; C-u C-SPCの後C-SPCだけでマークを遡れる
    (straight-vc-git-default-clone-depth . 150)
    (tramp-ssh-controlmaster-options . "-4") ; ssh接続時にipv4アドレスを利用する
    (tool-bar-mode . nil)
    (truncate-lines . t)         ;文字列を折り返さない
    (use-dialog-box . nil)
    (use-file-dialog . nil)
    (vc-follow-symlinks . t)
    )
  )

(leaf custom-darwin
  :if (eq system-type 'darwin)
  :custom
  ((browse-url-firefox-program . "/Applications/Firefox.app/Contents/MacOS/firefox")
   (browse-url-firefox-new-window-is-tab . t))
  )

(leaf authinfo
  :mode ("authinfo.gpg" . authinfo-mode))

(leaf browse-url
  :config
  (when (and (eq system-type 'gnu/linux)
             (string-match ".*-microsoft-standard-WSL2.*"
                           operating-system-release))
    (setq
     browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
     browse-url-generic-args     '("/c" "start")
     browse-url-browser-function #'browse-url-generic))
  (when (eq system-type 'darwin)
    (setq browse-url-browser-function #'browse-url-default-macosx-browser)))

(leaf deepl-translate
  :url "https://uwabami.github.io/cc-env/Emacs.html"
  :commands my:deepl-translate
  :preface
  (require 'url-util)
  (defun my:deepl-translate (&optional string)
    (interactive)
    (setq string
          (cond ((stringp string) string)
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (run-at-time 0.1 nil 'deactivate-mark)
    (let ((url (format "https://www.deepl.com/translator#en/ja/%s"
                       (url-hexify-string string))))
      (if (eq system-type 'darwin)
          (browse-url-default-macosx-browser url)
        (browse-url-generic url))))
  )

(leaf image-mode
  :bind (:image-mode-map
         ("=" . image-increase-size)))

(leaf completion
  :emacs>= 27
  :config
  (push 'flex completion-styles)
  )

(leaf help-mode
  :bind
  (:help-mode-map
   ("n" . next-line)
   ("j" . next-line)
   ("p" . previous-line)
   ("k" . previous-line)
   ("v" . scroll-up-command)
   ("V" . scroll-down-command)))

(leaf autorevert
  :hook
  (emacs-startup-hook . global-auto-revert-mode)
  )

(leaf initchart
  :disabled t
  :straight (initchart :type git :host github :repo "yuttie/initchart")
  :require t
  :config
  (initchart-record-execution-time-of load file)
  (initchart-record-execution-time-of require feature))

(defun which-linux-distribution ()
  "return string which obtains from 'lsb_release' command"
  (interactive)
  (if (eq system-type 'gnu/linux)
      (string-trim (shell-command-to-string "lsb_release -sd") "^\"" "\"?[ \t\n\r]+")
    ""))
(setq my:lsb-distribution-name
      (which-linux-distribution))

(recentf-mode 1)

;;行番号を表示
(if (version<= "26.0.50" emacs-version)
    (progn
      ;; (global-display-line-numbers-mode)
      (setq-default indicate-empty-lines t)
      (setq-default indicate-buffer-boundaries 'left)))

(leaf exec-path-from-shell
  :straight t
  :require t
  :config
  (add-to-list 'exec-path-from-shell-variables "PYTHONPATH")
  (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
  (exec-path-from-shell-initialize))

(leaf system-packages
  :straight t
  :require t
  :config
  (when (eq system-type 'darwin)
    (setq system-packages-use-sudo nil
          system-packages-package-manager 'brew))
  (when (or (string-match-p "arch" operating-system-release)
            (string-match-p "manjaro" operating-system-release)
            (string-match-p "endeavouros" my:lsb-distribution-name))
    (add-to-list 'system-packages-supported-package-managers
                 '(yay .
                          ((default-sudo . nil)
                           (install . "yay -S")
                           (search . "yay -Ss")
                           (uninstall . "yay -Rs")
                           (update . "yay -Syu")
                           (clean-cache . "yay -Sc")
                           (log . "cat /var/log/pacman.log")
                           (get-info . "yay -Qi")
                           (get-info-remote . "yay -Si")
                           (list-files-provided-by . "yay -Ql")
                           (verify-all-packages . "yay -Qkk")
                           (verify-all-dependencies . "yay -Dk")
                           (remove-orphaned . "yay -Rns $(pacman -Qtdq)")
                           (list-installed-packages . "yay -Qe")
                           (list-installed-packages-all . "yay -Q")
                           (list-dependencies-of . "yay -Qi")
                           (noconfirm . "--noconfirm"))))
    (setq system-packages-use-sudo nil
          system-packages-package-manager 'yay)
    )
  )


(leaf bind-key
  :bind
  (("C-c t l" . toggle-truncate-lines)
   ("C-t" . other-window)
   ("M-<f1>" . other-frame)  ;Macのショートカットに合わせる
   ("C-o" . my-insert-newline-and-indent)
   (:isearch-mode-map
    ("C-o" . isearch-exit))
   (:reb-mode-map
          :package re-builder
          ("C-c C-k". reb-quit))
   ))

(leaf special-characer-mode
  :url "https://github.com/madanh/special-characer-mode"
  :config
  (defmacro ins-val (val)
  `(lambda () (interactive) (insert ,val)))
  (define-minor-mode special-char-mode
    "Toggle Special Character mode"
    nil
    " SpecialChar"
    `(
      (,(kbd "1") . ,(ins-val "!")) (,(kbd "!") . ,(ins-val "1")) (,[kp-1] . ,(ins-val "1"))
      (,(kbd "2") . ,(ins-val "@")) (,(kbd "@") . ,(ins-val "2")) (,[kp-2] . ,(ins-val "2"))
      (,(kbd "3") . ,(ins-val "#")) (,(kbd "#") . ,(ins-val "3")) (,[kp-3] . ,(ins-val "3"))
      (,(kbd "4") . ,(ins-val "$")) (,(kbd "$") . ,(ins-val "4")) (,[kp-4] . ,(ins-val "4"))
      (,(kbd "5") . ,(ins-val "%")) (,(kbd "%") . ,(ins-val "5")) (,[kp-5] . ,(ins-val "5"))
      (,(kbd "6") . ,(ins-val "^")) (,(kbd "^") . ,(ins-val "6")) (,[kp-6] . ,(ins-val "6"))
      (,(kbd "7") . ,(ins-val "&")) (,(kbd "&") . ,(ins-val "7")) (,[kp-7] . ,(ins-val "7"))
      (,(kbd "8") . ,(ins-val "*")) (,(kbd "*") . ,(ins-val "8")) (,[kp-8] . ,(ins-val "8")) (,[kp-multiply] . ,(ins-val "*"))
      (,(kbd "9") . ,(ins-val "(")) (,(kbd "(") . ,(ins-val "9")) (,[kp-9] . ,(ins-val "9"))
      (,(kbd "0") . ,(ins-val ")")) (,(kbd ")") . ,(ins-val "0")) (,[kp-0] . ,(ins-val "0"))
      )))

(when (equal system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (when (or (eq window-system 'ns)
            (eq window-system 'mac))
    ;; 游教科書体
    ;; (set-face-attribute 'default nil
    ;;                     :family "YuKyokasho Yoko")
    ;; 源ノ角ゴシック
    (set-face-attribute 'default nil
                        :family "PlemolJP" :height 140)
    (let* ((variable-tuple
            (cond ((x-list-fonts "PlemolJP") '(:font "PlemolJP"))
                  ((x-list-fonts "Source Sans Pro")       '(:font "Source Sans Pro"))
                  ((x-list-fonts "Lucida Grande")         '(:font "Lucida Grande"))
                  ((x-list-fonts "Verdana")               '(:font "Verdana"))
                  ((x-family-fonts "Sans Serif")          '(:family "Sans Serif"))
                  (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
           (headline           `(:inherit default :weight bold)))

      (custom-theme-set-faces
       'user
       `(org-level-8 ((t (,@headline ,@variable-tuple))))
       `(org-level-7 ((t (,@headline ,@variable-tuple))))
       `(org-level-6 ((t (,@headline ,@variable-tuple))))
       `(org-level-5 ((t (,@headline ,@variable-tuple))))
       `(org-level-4 ((t (,@headline ,@variable-tuple))))
       `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
       `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
       `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
       `(org-table ((t (,@headline ,@variable-tuple))))
       `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
    )
  )
(when (equal system-type 'gnu/linux)
  (add-to-list 'load-path "~/opt/mu-1.0/mu4e/")
  ;;曖昧な文字幅を指定する
  (aset char-width-table ?→ 2)

  (when (eq window-system 'x)
     ;ディスプレイのサイズに応じて調節したい (x-display-pixel-width)
    
    (let ((font-height
           (cond
            ((> (x-display-pixel-width) 1680) 180)
            (t 100))))
      (set-face-attribute 'default nil :family "PlemolJP" :height font-height))
    ))

;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
(setq use-default-font-for-symbols nil)


(leaf restart-emacs :straight t)

(leaf dired*
  :config
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil)))

(leaf xwidget-webkit
  :hook
  ((xwidget-webkit-mode-hook . (lambda ()
                                 (display-line-numbers-mode -1)))))

(leaf pomodoro
  :doc "A timer for the Pomodoro Technique"
  :straight t
  :require t
  :config
  (when (eq window-system 'ns)
    (setq pomodoro-sound-player "afplay"))
  
  (let ((sound (cond
                ((or (string-match "Ubuntu" my:lsb-distribution-name)
                     (string-match "debian" my:lsb-distribution-name))
                 "/usr/share/sounds/gnome/default/alerts/glass.ogg")
                ((string-match "endeavouros" my:lsb-distribution-name)
                 "/usr/share/sounds/freedesktop/stereo/service-login.oga")
                ((eq window-system 'ns)
                 "/System/Library/Sounds/Glass.aiff"))))
    (setq pomodoro-work-start-sound sound
          pomodoro-break-start-sound sound))
  (pomodoro-add-to-mode-line))

(leaf sudo-edit :straight t)

(leaf so-long
  :doc "Say farewell to performance problems with minified code."
  :straight t
  :require t
)

(leaf projectile
  :bind ((:projectile-mode-map
          ("C-c p" . projectile-command-map)))
  :straight t
  :custom
  `((projectile-cache-file . ,(format "%sprojectile/%s/%s" user-emacs-directory emacs-version "projectile.cache"))
    (projectile-known-projects-file . ,(format "%sprojectile/%s/%s" user-emacs-directory emacs-version "projectile-bookmarks.eld"))
    (projectile-sort-order . 'recently-active))
  :init
  (let ((dir (format "%sprojectile/%s/" user-emacs-directory emacs-version)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    )
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
         )
  :mode ("jisyo" . skk-jisyo-edit-mode)
  :custom
  `((skk-japanese-message-and-error . t)
   (skk-share-private-jisyo . t)
   (skk-isearch-start-mode . 'latin); isearch で skk の初期状態
   (skk-user-directory . ,(format "%sddskk/" user-emacs-directory))
   (skk-use-jisx0201-input-method . t)
   (skk-henkan-strict-okuri-precedence . t)
   (skk-save-jisyo-instantly . t)
   (skk-sticky-key . '(117 101))
   (skk-egg-like-newline . t)           ;non-nilにするとEnterでの確定時に改行しない
   )
  :init
  (leaf skk-dropbox
    :if (file-exists-p "~/Dropbox/.config/ddskk/")
    :custom
    ((skk-jisyo . "~/Dropbox/.config/ddskk/jisyo")
     (skk-jisyo-code . 'utf-8)))
  (push (lambda ()
          (if (bolp)
              (org-at-heading-p)
            nil))
        context-skk-context-check-hook)
  (push (lambda ()
          (if (bolp)
               (org-at-block-p)
            nil))
        context-skk-context-check-hook)
  (push (lambda ()
          (if (bolp)
              (or (org-at-item-bullet-p)
                  (org-at-item-checkbox-p))
            nil))
        context-skk-context-check-hook)
  (setq skk-get-jisyo-directory (expand-file-name (format "%sskk-get-jisyo/" user-emacs-directory)))
  (let ((skk-jisyo-directory (if (file-exists-p "~/Dropbox/.config/ddskk/skkdic-utf8/")
                                 "~/Dropbox/.config/ddskk/skkdic-utf8/"
                               skk-get-jisyo-directory)))
    (setq skk-large-jisyo (format "%sSKK-JISYO.L" skk-jisyo-directory))
    (setq skk-extra-jisyo-file-list
          (mapcar (lambda (x)
                    (format "%s%s" skk-jisyo-directory x))
                  '("SKK-JISYO.lisp" "SKK-JISYO.station"
                    "SKK-JISYO.assoc" "SKK-JISYO.edict"
                    "SKK-JISYO.law" "SKK-JISYO.jinmei"
                    "SKK-JISYO.fullname" "SKK-JISYO.geo"
                    "SKK-JISYO.itaiji" "SKK-JISYO.zipcode"
                    "SKK-JISYO.okinawa" "SKK-JISYO.propernoun")))
    )
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
  (defun skk-set-display-table ()
    (walk-windows (lambda (w)
                    (let ((disptab (make-display-table)))
                      (aset disptab ?\▼ (vector (make-glyph-code ?# 'escape-glyph)))
                      (aset disptab ?\▽ (vector (make-glyph-code ?@ 'escape-glyph)))
                      (set-window-display-table w disptab)))))
  (add-hook 'window-configuration-change-hook #'skk-set-display-table)
  (add-hook 'after-init-hook #'skk-set-display-table))


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

(leaf *vertico
  :config
  
  (leaf vertico
    :emacs>= 27.1
    :straight (vertico :type git :host github :repo "minad/vertico"
                       :files (:defaults "extensions/*.el"))
    :bind ((:vertico-map
            ("M-RET" . minibuffer-force-complete-and-exit)
            ("M-TAB" . minibuffer-complete)
            ("C-r" . vertico-previous)
            ("C-s" . vertico-next)))
    :custom
    ((vertico-count . 20)
     (enable-recursive-minibuffers . t)
     (vertico-cycle . t)
     (vertico-resize . t)
     (minibuffer-prompt-properties
      . '(read-only t cursor-intangible t face minibuffer-prompt))
     )
    :init
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
    (vertico-mode)
    )
  (leaf vertico-multiform
    :disabled t
    :after consult vertico
    :config
    (vertico-multiform-mode)
    
    (setq vertico-multiform-commands
          `((consult-imenu buffer ,(lambda (_) (text-scale-set -1)))
            (consult-outline buffer ,(lambda (_) (text-scale-set -1)))))

    ;; Configure the buffer display and the buffer display action
    (setq vertico-multiform-categories
          '((consult-grep
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))))

    ;; Disable preview for consult-grep commands
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       :preview-key nil)
    )

  (leaf vertico-repeat
    :after vertico
    :bind ("M-r" . vertico-repeat)
    :hook
    (minibuffer-setup-hook . vertico-repeat-save))
  (leaf vertico-directory
    :after vertico
    :bind ((:vertico-map
            ("RET"    . vertico-directory-enter)
            ("DEL"    . vertico-directory-delete-char)
            ("M-DEL"  . vertico-directory-delete-word)
            ("C-l"    . vertico-directory-up))
           )
    ;; Tidy shadowed file names
    :hook
    (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))
  (leaf vertico-quick
    :after vertico
    :custom
    ((vertico-quick1 . "aoeu")
     (vertico-quick2 . "htns")))
  
  ;; Use the `orderless' completion style.
  ;; Enable `partial-completion' for files to allow path expansion.
  ;; You may prefer to use `initials' instead of `partial-completion'.
  (leaf orderless
    :straight t
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles basic partial-completion))))
    )

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (leaf savehist
    :init
    (savehist-mode))

  (leaf consult
    :straight t
    :custom
    ((consult-narrow-key . ">")
     (consult-find-command
      . "fd -H -E .git --color=never --full-path ARG OPTS")
     (consult-ripgrep-args
      . "rg --hidden --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
     (xref-show-xrefs-function . #'consult-xref)
     (xref-show-definitions-function . #'consult-xref))
    :bind (
           ("C-x C-SPC" . consult-global-mark)
           ("C-x b" . consult-buffer)
           ("C-x c i" . consult-imenu)
           ("C-x j" . consult-recent-file)
           ("C-x r j" . consult-register)
           ("C-x r l"  . consult-bookmark)
           ("M-y" . consult-yank-pop)
           ([remap goto-line] . consult-goto-line)
           (:isearch-mode-map
            ("C-i" . my-consult-line)
            ("M-e" . consult-isearch))
           )
    :hook
    (completion-list-mode-hook . consult-preview-at-point-mode)
    :config
    ;; https://github.com/minad/consult/wiki#find-files-using-fd
    (defvar consult--fd-command nil)
    (defun consult--fd-builder (input)
      (unless consult--fd-command
        (setq consult--fd-command
              (if (eq 0 (call-process-shell-command "fdfind"))
                  "fdfind"
                "fd")))
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (`(,re . ,hl) (funcall consult--regexp-compiler
                                          arg 'extended)))
        (when re
          (list :command (append
                          (list consult--fd-command
                                "--color=never" "--full-path"
                                (consult--join-regexps re 'extended))
                          opts)
                :highlight hl))))

    (defun consult-fd (&optional dir initial)
      (interactive "P")
      (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
             (default-directory (cdr prompt-dir)))
        (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

    (consult-customize
     consult-theme
     consult-goto-line consult-line
     :preview-key (list :debounce 0.2 'any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     consult-find consult-org-agenda
     :preview-key (kbd "C-,"))
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-root-function #'projectile-project-root)
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))
    (defun my-consult-line (&optional at-point)
      (interactive "P")
      (if at-point
          (consult-line (thing-at-point 'symbol))
        (consult-line)))
    )
  (leaf consult-projectile
    :after consult projectile
    :straight (consult-projectile
               :type git :host gitlab
               :repo "OlMon/consult-projectile" :branch "master")
    )
  (leaf affe
    :straight t
    :after consult orderless
    :custom
    ((affe-find-command . "fd -H -E .git --color=never --full-path")
     (affe-grep-command . "rg --hidden --color=never --max-columns=1000 --no-heading --line-number -v ^$ .")
     )
    
    
    :config
    ;; Configure Orderless
    (setq affe-regexp-function #'orderless-pattern-compiler
          affe-highlight-function #'orderless--highlight)

    ;; Manual preview key for `affe-grep'
    (consult-customize affe-grep :preview-key (kbd "M-.")))
  
  (leaf marginalia
    :straight t
    :bind (
           ("M-A" . marginalia-cycle)
           (:minibuffer-local-map
            ("M-A" . marginalia-cycle))
           )
    :init
    (marginalia-mode))

  (leaf embark
    :straight (embark :host github :repo "oantolin/embark" :branch "master" :files (:defaults))
    :emacs>= 26.1
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init

    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    :config

    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  ;; Consult users will also want the embark-consult package.
  (leaf embark-consult
    :after (embark consult)
    :require t
    :leaf-defer nil ;; :demand t
    ;; :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook
    (embark-collect-mode-hook . consult-preview-at-point-mode)
    ;; :init (with-eval-after-load 'embark
    ;;         (require 'embark-consult))
    )
  (leaf vertico-org-refile
    :after (org)
    :custom
    ((org-refile-use-outline-path . 'file)
     (org-outline-path-complete-in-steps . nil)))
  (leaf corfu
    :url "https://github.com/minad/corfu"
    :straight
    (corfu :type git :host github :repo "minad/corfu" :branch "main"
           :files ("*" (:exclude ".git")))
    :custom
    ((completion-cycle-threshold . 3)
     (corfu-auto . t)
     (corfu-cycle . t)
     (corfu-excluded-modes . '(rustic-mode)) ;major-modeを指定する
     (tab-always-indent . 'complete))
    :init
    (corfu-global-mode)
    )
  
  (leaf cape
    :doc "Completion At Point Extensions"
    :req "emacs-27.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/cape"
    :added "2021-11-30"
    :emacs>= 27.1
    :straight t
    :bind
    (("C-c f p" . completion-at-point) ;; capf
     ("C-c f t" . complete-tag)        ;; etags
     ("C-c f d" . cape-dabbrev)        ;; or dabbrev-completion
     ("C-c f f" . cape-file)
     ("C-c f k" . cape-keyword)
     ("C-c f s" . cape-symbol)
     ("C-c f a" . cape-abbrev)
     ("C-c f i" . cape-ispell)
     ("C-c f l" . cape-line)
     ("C-c f w" . cape-dict))
    :init
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-file)
    :config
    (if (memq system-type '(darwin gnu/linux))
        (customize-set-variable 'cape-dict-file "/usr/share/dict/words")))
  (leaf kind-icon
    :emacs>= 27.1
    :straight (kind-icon :type git :host github :repo "jdtsmith/kind-icon"
                         :files ("*" (:exclude ".git"))
                         :branch "main")
    :require t
    :after (corfu svg-lib)
    :custom
    (kind-icon-default-face . 'corfu-default)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    )
  )


(leaf magit
  :bind (("C-x g" . magit-status))
  :require t
  :straight t
  :custom
  ((magit-display-buffer-function . 'magit-display-buffer-fullframe-status-v1)
   (magit-diff-refine-hunk . 'all))
  :config
  ;; ediff時にorgファイルを全て表示する
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all)))
(leaf magit-svn
  :straight t)
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
               "/opt/homebrew/opt/cmigemo/share/migemo/utf-8/migemo-dict")
              ((eq system-type 'windows-nt)
               "~/opt/cmigemo-default-win64/dict/utf-8")
              ((string-match-p "arch" operating-system-release)
               "/usr/share/migemo/utf-8/migemo-dict")
              (t "/usr/share/cmigemo/utf-8/migemo-dict")))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init))


;; SLIMEのロード


(leaf undo-tree
  :straight (undo-tree :type git :host gitlab :repo "tsc25/undo-tree")
  :diminish (global-undo-tree-mode undo-tree-mode)
  :require t
  :global-minor-mode global-undo-tree-mode
  :custom (undo-tree-history-directory-alist . '(("." . "~/.emacs.d/undo-tree/"))))

(leaf rust-mode
  :doc "A major-mode for editing Rust source code"
  :req "emacs-25.1"
  :tag "languages" "emacs>=25.1"
  :url "https://github.com/rust-lang/rust-mode"
  :emacs>= 25.1
  :straight t
  :hook (rust-mode-hook . (lambda () (prettify-symbols-mode)))
  :config
  (push '(".add" . ?∔) rust-prettify-symbols-alist)
  )

(leaf rustic   :straight t
  :doc "Rust development environment"
  :req "emacs-26.1" "rust-mode-1.0.3" "dash-2.13.0" "f-0.18.2" "let-alist-1.0.4" "markdown-mode-2.3" "project-0.3.0" "s-1.10.0" "seq-2.3" "spinner-1.7.3" "xterm-color-1.6"
  :tag "languages" "emacs>=26.1"
  :emacs>= 26.1
  :after rust-mode markdown-mode project spinner xterm-color
  :custom (
           (rustic-lsp-server . 'rust-analyzer)
           ;; (rustic-lsp-client . 'eglot)
           )
  )

(leaf ron-mode
  :doc "Rusty Object Notation mode"
  :req "emacs-24.5.1"
  :tag "languages" "emacs>=24.5.1"
  :url "https://chiselapp.com/user/Hutzdog/repository/ron-mode/home"
  :added "2021-12-26"
  :emacs>= 24.5
  :straight t
  :mode (("\\.ron$" . ron-mode)))


(leaf moody
  :straight t
  :custom
  ((x-underline-at-descent-line . t))
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(leaf dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(leaf wgrep
  :require t
  :straight t
  :custom
  ((wgrep-enable-key . "e")
   (wgrep-auto-save-buffer . t))
  )

(leaf highlight-symbol
  :straight t
  :require t)

(leaf expand-region
  :straight t
  :require t
  :bind (("C-=" . er/expand-region)))

(leaf all-the-icons :straight t)

(leaf which-key :straight t
  :diminish t
  :custom
  (which-key-idle-secondary-delay . 0.0)
  (which-key-max-description-length . 35)
  :config
  ;; 3つの表示方法どれか1つ選ぶ
  (which-key-setup-side-window-bottom)    ;ミニバッファ
  ;; (which-key-setup-side-window-right)     ;右端
  ;; (which-key-setup-side-window-right-bottom) ;両方使う
  (which-key-mode 1))

;;;yasnippet
(leaf yasnippet
  :straight t
  :require t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(leaf yasnippet-snippets :straight t)

(leaf gitignore-templates
  :doc "Create .gitignore using GitHub or gitignore.io API"
  :req "emacs-24.3"
  :tag "tools" "emacs>=24.3"
  :url "https://github.com/xuchunyang/gitignore-templates.el"
  :added "2021-12-17"
  :emacs>= 24.3
  :straight t)

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
  :require t
  :straight t
  :mode (("\\.gradle$" . gradle-mode)))


(leaf slime
  :emacs< "28"
  :if (file-exists-p "~/.roswell/helper.el")
  :custom
  ((slime-auto-start . 'ask)
   )
  :hook ((lisp-mode-hook . slime-mode)
         )
  :init
  (load (expand-file-name "~/.roswell/helper.el"))
  :config
  ;; (slime-setup '(slime-fancy slime-company))
  (leaf slime-company
    :straight t
    :custom ((slime-company-completion . 'fuzzy)
             (slime-complete-symbol*-fancy . t))
    :hook ((slime-repl-mode-hook
            . (lambda () (add-to-list
                          'company-backends
                          '(company-slime company-dabbrev-code))))))
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
  ;; :mode (("\\.as[cp]x\\'"    . web-mode)
  ;;        ("\\.djhtml\\'"     . web-mode)
  ;;        ("\\.erb\\'"        . web-mode)
  ;;        ("\\.html?\\'"      . web-mode)
  ;;        ("\\.mustache\\'"   . web-mode)
  ;;        ("\\.php\\'"        . web-mode)
  ;;        ("\\.phtml\\'"      . web-mode)
  ;;        ("\\.tpl\\.php\\'"  . web-mode)
  ;;        ("\\.[gj]sp\\'"     . web-mode))
  :config
  (setq web-mode-extra-snippets
        '(("php" . (("print" . "print(\"|\")"))))))

;; Org-mode
(leaf org*
  :config
  (leaf org
    :commands ((org-at-item-bullet-p))
    :mode (("\\.org$" . org-mode))
    :straight
    (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                    :local-repo "org" :depth 300 :branch "main"
                    :pre-build
                    (straight-recipes-org-elpa--build) :build (:not autoloads)
                    :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")))
    :custom
    ((org-export-allow-bind-keywords . t)
     (org-export-backends . '(ascii html icalendar latex md odt taskjuggler asciidoc pandoc gfm))
     (org-id-link-to-org-use-id . t)
     (org-icalendar-use-scheduled . '(event-if-todo todo-start))
     (org-link-file-path-type . 'relative)
     (org-list-allow-alphabetical . t)
     (org-return-follows-link . t)
     (org-agenda-start-on-weekday . 0)
     (org-link-frame-setup .
                           '((vm . vm-visit-folder-other-frame)
                             (vm-imap . vm-visit-imap-folder-other-frame)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl-other-frame)))
     (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
     (org-src-lang-modes . '(("C" . c)
                             ("C++" . c++)
                             ("arduino" . arduino)
                             ("asm" . asm)
                             ("asymptote" . asy)
                             ("bash" . sh)
                             ("browser" . html)
                             ("browser" . web)
                             ("calc" . fundamental)
                             ("cpp" . c++)
                             ("ditaa" . artist)
                             ("dot" . graphviz-dot)
                             ("elisp" . emacs-lisp)
                             ("html" . web)
                             ("ocaml" . tuareg)
                             ("php" . php)
                             ("python" . python)
                             ("redis" . redis)
                             ("rust" . rustic)
                             ("screen" . shell-script)
                             ("shell" . sh)
                             ("sqlite" . sql)
                             ))
     (org-src-preserve-indentation . t)
     (org-startup-folded . t)
     (org-preview-latex-default-process . 'dvisvgm)
     (org-clock-persist . t)
     )
    :bind (("C-c c" . org-capture)
           ("C-c l" . org-store-link)
           ("C-c a" . org-agenda)
           ("<f2>" . insert-zero-width-space)
           (:org-mode-map
            ("C-c C-\'" . org-insert-structure-template)))
    :init
    (defvar my:org-item-key-bindings
      '(("p" . org-previous-item)
        ("n" . org-next-item)
        ("U" . org-metaup)
        ("D" . org-metadown)
        ("r" .   org-metaright)
        ("l" .   org-metaleft)
        ("R" .   org-shiftmetaright)
        ("L" .   org-shiftmetaleft)
        ("t" . org-toggle-checkbox)
        ("i" . (lambda () (org-insert-item) (org-move-item-down) (org-beginning-of-line)))
        ("c" . (lambda ()  (org-insert-item t) (org-move-item-down) (org-beginning-of-line)))
        ("Clock Commands")
        ("I" . org-clock-in)
        ("O" . org-clock-out)
        ("?" . (progn
                 (with-output-to-temp-buffer "*Help*"
                   (princ "Speed commands\n==============\n")
                   (mapc #'org-print-speed-command
                         ;; FIXME: don't check `org-speed-commands-user' past 9.6
                         my:org-item-key-bindings))
                 (with-current-buffer "*Help*"
                   (setq truncate-lines t)))
         )))
    (defun my:org-item-speed-command-activate (keys)
      (when (and (bolp)
                 (org-at-item-p))
        (cdr (assoc keys my:org-item-key-bindings))))
    
    (defun insert-zero-width-space()
      (interactive)
      (insert-char #x200b))
    (defun insert-zero-width-space-twice()
      (interactive)
      (insert-zero-width-space)
      (insert-zero-width-space))
    (setq org-directory
          (expand-file-name
           (if (file-exists-p "~/git/notes/")
               "~/git/notes/"
             (progn
               (when(not (file-exists-p "~/org/"))
                 (mkdir "~/org/"))
               "~/org/"))))
    :config
    
    ;; org-habitモジュールを有効化
    (add-to-list 'org-modules 'org-habit)
    (add-to-list 'org-modules 'org-id)

    (push 'my:org-item-speed-command-activate
          org-speed-command-hook)
    (org-clock-persistence-insinuate)
    ;; 強調の規則を変更(別の環境で開いた場合は認識されなくなる...)
    (setcar org-emphasis-regexp-components "-[:space:]\x200B('\"{")
    (setcar (nthcdr 1 org-emphasis-regexp-components) "-[:space:]\x200B.,:!?;'\")}\\[")
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
    
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 2.0))
    ;; org-modeの固定幅フォントを設定
    (let ((fontset (cond
                    ((eq window-system 'ns) "PlemolJP")
                    ((eq window-system 'x) "PlemolJP"))))
      (dolist (face '(org-table
                      org-formula
                      org-date))
        (set-face-attribute face nil :family fontset)))

    (add-to-list 'face-font-rescale-alist
                 '(".*IPAゴシック.*" . 0.85))

    (when (equal system-type 'darwin)
      (setq org-plantuml-jar-path
            "/usr/local/opt/plantuml/libexec/plantuml.jar"))


    
    
    (setq org-refile-targets
          `((nil . (:maxlevel . 2))
            (org-agenda-files . (:maxlevel . 2))
            (,(concat org-directory "calendar/") . (:maxlevel . 2))
            ))
    (setq org-tag-alist
          '(("ignore" . ?i) ("@OFFICE" . ?o) ("@HOME" . ?h) ("SHOPPING" . ?s)
            ("MAIL" . ?m) ("PROJECT" . ?p) ("備忘録" . ?b)))
    (setq org-capture-templates
          `(("i" "インボックス" entry
             (file ,(concat org-directory "inbox.org"))
             "* %? %i\n%U\n")
            ;; ("h" "定期的にやること" entry
            ;;  (file ,(concat org-directory "habit.org"))
            ;;  "* %?\n %U\n")
            ("t" "タスク" entry
             (file ,(concat org-directory "task.org"))
             "* TODO %? %i\n%U\n")
            ("e" "イベント" entry
             (file ,(concat org-directory "event.org"))
             "* EVENT %?\n %a\n%U\n")
            ("n"
             "ノート(本文から書く)"
             entry
             (file+headline, (concat org-directory "notes.org") "MEMO")
             "* %U \n%?")
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
             (file+olp+datetree (concat org-directory "minutes.org") "会議")
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
                                 (php        . t)
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

  (leaf org-agenda
    :after org
    :config
    (add-to-list 'org-agenda-files (format "%s%s" org-directory "agenda/"))
    (add-to-list 'org-agenda-files (format "%s%s" org-directory "calendar/"))
    )

  (leaf org-contrib
    :after org
    :straight (org-contrib :type git :repo "https://git.sr.ht/~bzg/org-contrib"))

  (leaf org-mu4e
    :disabled t
    :straight t
    :after (org mu4e)
    :config
    ;;store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil))
  (leaf ox-rst
    :straight t
    :after (org)
    :custom
    ((org-rst-headline-underline-characters . '(45 126 94 58 39 32 95))))
  (leaf ob-browser
    :straight t
    :after org)
  (leaf ob-java
    :custom
    ((org-babel-java-compiler . "javac -encoding UTF-8")))
  (leaf ox-epub
    :straight t
    :after org)
  (leaf ox*
    :after org
    :custom
    (org-export-allow-bind-keywords . t)
    :config
    (defvar org-export-directory nil
      "org-exportの出力先を指定する変数。buffer-local変数として指定する。")
    (defun org-export-output-file-name--set-directory (orig-fn extension &optional subtreep pub-dir)
      (setq pub-dir (or pub-dir org-export-directory))
      (funcall orig-fn extension subtreep pub-dir))
    (advice-add 'org-export-output-file-name :around 'org-export-output-file-name--set-directory)
    (leaf ox-pandoc
      :after org
      :straight t
      :require t
      :if (or (file-exists-p "/usr/bin/pandoc")
              (file-exists-p "/usr/local/bin/pandoc")
              (file-exists-p "/opt/local/bin/pandoc")
              (file-exists-p "/opt/homebrew/bin/pandoc"))))
  
  
  (leaf org-brain
    :straight t
    :after org
    :require t
    :bind
    ((:org-mode-map
      ("C-c b" . org-brain-prefix-map)))
    )

  (leaf org-roam
    :req "emacs-26.1" "dash-2.13" "f-0.17.2" "org-9.4" "emacsql-3.0.0" "emacsql-sqlite-1.0.0" "magit-section-3.0.0"
    :emacs>= 26.1
    :straight t
    :commands (org-roam-node-find)
    :custom
    ((org-roam-title-to-slug-function . (lambda (text) text))
     (org-roam-v2-ack . t))
    :bind
    (("C-c n l" . org-roam-buffer-toggle)
     ("C-c n f" . org-roam-node-find)
     ("C-c n g" . org-roam-graph)
     ("C-c n i" . org-roam-node-insert)
     ("C-c n c" . org-roam-capture)
     ;; Dailies
     ("C-c n j" . org-roam-dailies-capture-today)
     ;; (:org-roam-mode-map
     ;;  ("C-c n l" . org-roam)
     ;;  ("C-c n f" . org-roam-find-file)
     ;;  ("C-c n g" . org-roam-graph)
     ;;  ("C-c n t a" . org-roam-tag-add)
     ;;  ("C-c n t d" . org-roam-tag-delete))
     ;; (:org-mode-map
     ;;  ("C-c n i" . org-roam-insert)
     ;;  ("C-c n I" . org-roam-insert-immediate))
     )
    :config
    (setq org-roam-directory (format "%sroam/" org-directory))
    (org-roam-db-autosync-mode)
    (when (eq system-type 'darwin)
      (setq org-roam-graph-viewer "open"))
    (add-to-list 'org-roam-capture-templates
                 '("n" "note(default + headline)"
                   plain #'org-roam-capture--get-point
                   "%?"
                   :file-name "%<%Y%m%d%H%M%S>-${slug}.org"
                   :head "#+title: ${title}\n* Overview\n"
                   :unnarrowed t)
                 )
    (leaf org-roam-protocol
      :require t
      :after org
      )
    ;; (leaf org-roam-ui
    ;;   :straight (org-roam-ui :type git :host github :repo "org-roam/org-roam-ui")
    ;;   )
    (leaf org-roam-ui
      :req "emacs-27.1" "org-roam-2.0.0" "simple-httpd-20191103.1446" "websocket-1.13"
      :emacs>= 27.1
      :after org
      :straight
      (org-roam-ui :host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
      :hook (after-init-hook . org-roam-ui-mode)
      :custom ((org-roam-ui-sync-theme . t)
               (org-roam-ui-follow . t)
               (org-roam-ui-update-on-save . t)
               ;; (org-roam-ui-open-on-start . t)
               ))
    )

  (leaf org-journal
    :straight t
    :require t
    :after org
    :commands org-journal-new-entry
    :custom
    `((org-journal-file-type . 'daily)
      
      (org-journal-enable-agenda-integration . t)
      (org-journal-date-format . "%F (%a)")
      (org-journal-time-format . "<%Y-%m-%d %R> ")
      (org-journal-file-format . "%Y%m%d.org")
      (org-journal-file-header . "# -*- mode: org-journal; -*-"))
    :config
    (setq org-journal-dir (concat org-directory "journal/")))

  ;; Org Mode LaTeX Export

  (leaf org-eldoc
    :after org
    :require t
    :hook (org-mode-hook . eldoc-mode)
    :config
    (defadvice org-eldoc-documentation-function (around add-field-info activate)
      (or
       (ignore-errors (and (not (org-at-table-hline-p))
                           (org-table-field-info nil)))
       ad-do-it))
    (eldoc-add-command-completions
     "org-table-next-" "org-table-previous" "org-cycle"))

  (leaf ox-latex
    :require t
    :straight nil
    :after (org)
    :custom ((org-latex-minted-options . '(("frame" "single")
                                           ("breaklines" "")
                                           ("style" "xcode")
                                           ("fontsize" "\\footnotesize")))
             (org-latex-compiler . "lualatex")
             (org-latex-default-class . "lualatex-jlreq")
             (org-latex-listings . 'minted)
             (org-latex-listings-options . '(("frame" "single")
                                             ("basicstyle" "{\\ttfamily\\scriptsize}")
                                             ("numbers" "left")
                                             ("commentstyle" "{\\ttfamily\\scriptsize}")
                                             ("breaklines" "true")
                                             ("showstringspaces" "false")))
             (org-latex-minted-langs . '((rust "rust")
                                         (emacs-lisp "common-lisp")
                                         (cc "c++")
                                         (cperl "perl")
                                         (shell-script "bash")
                                         (caml "ocaml")
                                         (bash "bash")
                                         (conf "ini")))
             (org-preview-latex-default-process . 'dvisvgm)
             )
    :config
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
            '(("pdf" . "open -a Skim %s")
              ("php". emacs))))
    (when (equal system-type 'gnu/linux)
      (setq org-file-apps
            '(("pdf" . "evince %s"))))
    (add-to-list 'org-latex-classes
                 '("lualatex-jlreq"
                   "\\documentclass[]{jlreq}
\\usepackage{luatexja} % ltjclasses, ltjsclasses を使うときはこの行は不要
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
    )

  (leaf ox-taskjuggler
    :custom
    ((org-taskjuggler-process-command . "tj3 --silent --no-color --output-dir %o %f && open %o/Plan.html")))
  (leaf ox-gfm
    :straight (ox-gfm :type git :host github :repo "conao3/ox-gfm")
    :require t
    :after org)
  (setq org-ditaa-jar-path
        "/usr/local/opt/ditaa/libexec/ditaa-0.11.0-standalone.jar")

  (leaf ox-extra
    :after org
    :require t
    :config
    ;; ignoreタグで見出しを非表示にしつつ内容を表示する
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))
  (leaf ob-kotlin
    :after (org))
  (leaf ox-asciidoc
    :straight t
    :require t
    :after (org))
  (leaf ox-hugo
    :disabled t
    :straight (ox-hugo :type git :host github :repo "kaushalmodi/ox-hugo" :branch "main")
    :after org
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


  (leaf org-download
    :straight t
    :after org
    :require t
    :hook ((org-mode-hook . org-download-enable)))
  (leaf org-seek
    :commands (org-seek-string org-seek-regexp org-seek-headlines)
    ;;  :ensure-system-package (rg . ripgrep)
    :custom
    ((org-seek-search-tool . 'ripgrep)))

  (leaf org-pdf*
    :config
    (leaf org-pdftools
      :after org
      :straight (org-pdftools :type git :host github :repo "fuxialexander/org-pdftools")
      :custom
      `((org-pdftools-root-dir . ,(concat (getenv "HOME") "/GoogleDrive/Books")))
      :hook (org-mode-hook . org-pdftools-setup-link)
      )
    (leaf org-noter
      :after (org))
    (leaf org-noter-pdftools
      :straight (org-noter-pdftools :type git :host github :repo "fuxialexander/org-pdftools")
      :after (org-noter)
      :require t)
    (leaf pdf-tools
      :straight t
      ;; https://github.com/politza/pdf-tools#installation
      :mode (("\\.pdf\\'" . pdf-view-mode))
      :hook (pdf-view-mode-hook . (lambda ()
                                    (display-line-numbers-mode 0)))
      :custom ((pdf-view-use-scaling . t))
      :config
      (pdf-tools-install)
      (display-line-numbers-mode -1)
      (setq pdf-annot-activate-created-annotations t)
      (setq pdf-view-resize-factor 1.1)))
  (leaf org-re-reveal
    :straight t
    :after org)
  (leaf org-gcal
    :if (file-exists-p "~/Dropbox/org/googlecalendar/org-gcal-config.el")
    :straight t
    :after org
    :require t
    :custom
    ((org-gcal-down-days . 180)
     (org-gcal-up-days . 180))
    :config
    (load "~/Dropbox/org/googlecalendar/org-gcal-config.el"))
  (leaf ox-slimhtml
    :after org
    :straight t
    :require t)
  )

(leaf mu4e
  :disabled t
  :straight t
  :load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e"
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
  :diminish company-mode
  :bind ((:company-mode-map
          ("C-M-i" . company-complete-common-or-cycle))
         (:company-active-map
          ("C-n"   . company-select-next)
          ("C-p"   . company-select-previous)
          ("C-s"   . company-filter-candidates)
          ("C-i"   . company-complete-selection))
         (:company-search-map
          ("C-n"   . company-select-next)
          ("C-p"   . company-select-previous)))
  :hook (
         (c-mode-hook            . company-mode)
         (shell-script-mode-hook . company-mode)
         (sh-mode-hook           . company-mode)
         (shell-mode-hook        . company-mode)
         )
  :custom
  ((company-idle-delay . 0.2)
   (company-minimum-prefix-length . 2)
   (company-selection-wrap-around . t)
   )
  :config
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
  ;; (setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -shell-escape -norc -gg -pdfdvi")
  (setq tex-command "latexmk -e '$lualatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
  (setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
  (setq makeindex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
  (cond ((eq system-type 'darwin)
         (setq dvi2-command "open -a Skim"
               tex-pdfview-command "open -a Skim"
               dviprint-command-format
               "open -a \"Adobe Acrobat Reader DC\" `echo %s | gsed -e \"s/\\.[^.]*$/\\.pdf/\"`"))
        (t (setq dvi2-command "evince"
                tex-pdfview-command "evince")))
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
  :custom
  ((php-manual-url . 'ja)))
(leaf ac-php
  :straight t
  )
(leaf flycheck-phpstan
  :straight t
  :hook (php-mode-hook . (lambda ()
                           (require 'flycheck-phpstan)
                           (flycheck-mode t))))
(leaf company-php
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

(leaf rainbow-mode
  :straight t)


(leaf poetry
  :straight t
  :require t)

(leaf pipenv
  :disabled t
  :hook (python-mode-hook . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))




(leaf hydra :straight t)

(leaf go-mode
  :straight t
  :after lsp-mode
  :hook (go-mode-hook . lsp-deferred))

(leaf android-mode
  :straight t
  :disabled t)

(leaf ccls :straight t
  :after lsp-mode
  ;; :ensure-system-package ccls
  :hook ((c-mode-hook c++-mode-hook objc-mode-hook) .
         (lambda () (require 'ccls) (lsp-deferred)))
  :config
  (when (eq system-type 'darwin)
    (when (executable-find  "/usr/local/opt/ccls/bin/ccls")
      (setq ccls-executable "/usr/local/opt/ccls/bin/ccls"))
    (when (executable-find  "/opt/homebrew/opt/ccls/bin/ccls")
      (setq ccls-executable "/opt/homebrew/opt/ccls/bin/ccls"))
    (when (executable-find  "/opt/local/bin/ccls-clang-11")
      (setq ccls-executable "/opt/local/bin/ccls-clang-11"))
    ))



(leaf smartparens
  :straight t
  :diminish t
  :require smartparens-config
  :hook (after-init-hook . smartparens-global-mode)
  :bind
  (:emacs-lisp-mode-map
    ("C-c C-u" . sp-backward-up-sexp)))

(leaf kotlin-mode
  :straight t
  :mode (("\\.kt\\'" . kotlin-mode)))

(leaf whitespace
  :require t
  :diminish global-whitespace-mode
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
  :custom
  ((plantuml-default-exec-mode . 'jar)
   (plantuml-output-type . "png"))
  :config
  (setq plantuml-jar-path
        (cond ((eq system-type 'darwin)
                "/usr/local/opt/plantuml/libexec/plantuml.jar")
              ((string-match "ndeavour" my:lsb-distribution-name)
               "/usr/share/java/plantuml/plantuml.jar")
              (t ""))))

(leaf htmlize :straight t)
(leaf adoc-mode :straight t)
(leaf pandoc :straight t)
(leaf graphviz-dot-mode :straight t)
(leaf editorconfig
  :straight t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))
(leaf easy-hugo
  :disabled t
  :custom
  ((easy-hugo-org-header . t)
   (easy-hugo-default-ext . ".org")))
(leaf npm-mode
  :disabled t
  :straight t
  ;; :ensure-system-package npm
  )
(leaf autodisass-java-bytecode
  :straight t)

(leaf google-c-style
  :straight t
  :commands
  (google-set-c-style))
(leaf regex-tool :straight t)

(leaf solarized-theme
  :straight t
  :when (window-system)
  :config
  ;; (load-theme 'solarized-dark t)
  ;; (load-theme 'solarized-iceberg-dark t)
  )
(leaf modus-themes
  :straight t
  :require t
  :custom
  ((modus-themes-italic-constructs . t)
   (modus-themes-region . '(bg-only no-extend))
   (modus-themes-paren-match . '(bold intense))
   (modus-themes-org-blocks . 'gray-background)
   (modus-themes-mode-line . '(borderless accented))
   (modus-themes-vivendi-color-overrides . '((bg-main . "gray20")))
   )
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

(leaf markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom ((markdown-command . "multimarkdown"))
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



(leaf flycheck :straight t)
(leaf gnuplot :straight t)

(leaf *gdb
  ;;; GDB 関連
  :hook
  ((gdb-mode-hook . (lambda () (gud-tooltip-mode t))) ; 変数の上にマウスカーソルを置くと値を表示
   )
  :custom
  `((gdb-many-windows . t)              ; 有用なバッファを開くモード
    ;;; I/O バッファを表示
    (gdb-use-separate-io-buffer . t)
    ;;; t にすると mini buffer に値が表示される
    (gud-tooltip-echo-area . nil)
    ;;; バックアップファイルを作成しない
    (make-backup-files .t))
  )


(leaf asm-mode
  :hook ((asm-mode-set-comment-hook . (lambda ()
                                        (setq asm-comment-char ?#)))))
(leaf ssh-config-mode
  :straight t)
(leaf fish-mode
  :straight t)
(leaf dockerfile-mode
  :straight t
  :require t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
(leaf meson-mode
  :straight t
  )
(leaf git-modes
  :straight t
  :require t)
(leaf go-mode
  :straight t
  :hook ((go-mode-hook . lsp-deferred)))
(leaf groovy-mode
  :straight t)

(leaf server
  :commands (server-running-p)
  :hook
  (emacs-startup-hook . (lambda ()
                          (unless (server-running-p)
                            (server-start))))
  )

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
(leaf ispell
  :require t
  :config
  ;; (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  )
(leaf epg-config
  :custom
  (epg-pinentry-mode . 'loopback))
(leaf vimrc-mode
  :straight t
  :require t
  :mode
  ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(leaf *lsp
  :config
  
  (leaf lsp-mode
    :straight t
    :require t
    :commands (lsp lsp-deferred)
    :custom ((lsp-auto-execute-action . nil)
             (lsp-keymap-prefix . "C-c C-l"))
    :hook ((lsp-mode-hook  . lsp-enable-which-key-integration)
           (c-mode-hook    . lsp-deferred)
           (css-mode-hook  . lsp-deferred)
           (html-mode-hook . lsp-deferred))
    :init
    (setq read-process-output-max (* 1024 1024))
    (setq garbage-collection-messages t))
  
  (leaf lsp-python-ms
    :disabled t
    :straight t
    :require t
    :custom
    ((lsp-python-ms-python-executable-cmd . "python3"))
    :hook ((python-mode-hook . (lambda ()
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
  (leaf lsp-pyright
    :straight t
    :hook ((python-mode-hook . (lambda ()
                                 (require 'lsp-pyright)
                                 (when (file-exists-p
                                        (concat (projectile-project-root buffer-file-name) ".venv/"))
                                   (setq lsp-pyright-extra-paths
                                         (vector
                                          (format
                                           "%s/site-packages"
                                           (car
                                            (last (directory-files
                                                   (concat
                                                    (projectile-project-root buffer-file-name)
                                                    ".venv/lib/")
                                                   t))))))
                                   (message "lsp-pyright-extra-paths `%s'" lsp-pyright-extra-paths))
                                 (lsp-deferred))))
    
    :config
    (dolist (dir '(
                   "[/\\\\]\\.venv$"
                   "[/\\\\]\\.mypy_cache$"
                   "[/\\\\]__pycache__$"
                   ))
      (push dir lsp-file-watch-ignored))
    )
  
  ;; optionally
  (leaf lsp-ui
    :straight t
    :after lsp-mode
    :custom
    (lsp-ui-doc-show-with-cursor . t)
    
    (lsp-ui-doc-enable                  . t)
    (lsp-ui-doc-header                  . t)
    (lsp-ui-doc-include-signature       . t)
    (lsp-ui-doc-position                . 'bottom) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width               . 85)
    (lsp-ui-doc-max-height              . 20)
    (lsp-ui-doc-use-childframe          . t)
    (lsp-ui-doc-use-webkit              . nil)

    (lsp-ui-sideline-enable             . t)
    (lsp-ui-sideline-ignore-duplicate   . t)
    (lsp-ui-sideline-show-symbol        . t)
    (lsp-ui-sideline-show-hover         . t)
    (lsp-ui-sideline-show-diagnostics   . t)
    (lsp-ui-sideline-show-code-actions  . t)
    :bind ((:lsp-ui-mode-map
            ("M-." . lsp-ui-peek-find-definitions)
            ("M-?" . lsp-ui-peek-find-references))
           (:lsp-command-map
            ("t" . lsp-ui-doc-focus-frame)))
    )
  (leaf lsp-treemacs
    :commands lsp-treemacs-errors-list
    :config
    (lsp-treemacs-sync-mode 1))
  ;; optionally if you want to use debugger
  (leaf lsp-java
    :straight t
    :require t
    :hook (java-mode-hook . (lambda ()
                              (lsp-deferred)
                              (setq lsp-managed-mode t)))
    :bind ((:lsp-mode-map
            ("M-." . lsp-find-definition))))
  (leaf lsp-metals
    :straight t
    :custom
    ((lsp-metals-server-args . '("-J-Dmetals.allow-multiline-string-formatting=off")))
    :hook (scala-mode-hook . lsp-deferred)
    )

  (leaf lsp-dart
  :doc "Dart support lsp-mode"
  :req "emacs-26.3" "lsp-treemacs-0.3" "lsp-mode-7.0.1" "dap-mode-0.6" "f-0.20.0" "dash-2.14.1" "dart-mode-1.0.5"
  :tag "extensions" "languages" "emacs>=26.3"
  :url "https://emacs-lsp.github.io/lsp-dart"
  :emacs>= 26.3
  :straight t
  :after lsp-treemacs lsp-mode dap-mode dart-mode)
  
  (leaf dap-mode
    :straight t
    :after lsp-mode
    :config
    (dap-mode 1)
    (dap-ui-mode 1)
    (require 'dap-cpptools)
    (require 'dap-gdb-lldb)
    (dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                		   :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))
    (leaf dap-java
      :require t
      :after (lsp-java)))

  (leaf consult-lsp
    :straight t
    :after (consult lsp-mode)
    :config
    (consult-customize
     consult-lsp-symbols
     :preview-key (kbd "C-,"))
    )
  )

(leaf eglot
  :straight t
  :hook
  ((rustic-mode-hook . company-mode))
  :config
  ;; (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
  ;; (add-hook 'rustic-mode-hook 'eglot-ensure)
  )
(leaf vterm
  :straight t)
(leaf elfeed
  :doc "an Emacs Atom/RSS feed reader"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :url "https://github.com/skeeto/elfeed"
  :added "2021-12-03"
  :emacs>= 24.3
  :straight t
  :bind (:elfeed-search-mode-map
         ("j" . next-line)
         ("k" . previous-line))
  :custom
  (elfeed-search-date-format . '("%Y-%m-%d %H:%M" 16 :left))
  )
(leaf powershell
  :doc "Mode for editing PowerShell scripts"
  :req "emacs-24"
  :tag "languages" "powershell" "emacs>=24"
  :url "http://github.com/jschaf/powershell.el"
  :added "2021-12-04"
  :emacs>= 24
  :straight t)

(dolist (file (cddr (directory-files (concat
                                      user-emacs-directory
                                      "lisp/"))))
  (load-file (concat user-emacs-directory "lisp/" file)))


;; 読み込み専用で開く設定を持ったクラスを定義
(dir-locals-set-class-variables
 'read-only
 '((nil .((buffer-read-only . t)))))
;; クラスをディレクトリに関連づける
(dolist (dir
         (mapcar (lambda (str)
                   (format "%spackages/%s/straight/repos/"
                           user-emacs-directory str))
                 (cddr (directory-files
                        (concat user-emacs-directory "packages/")))
                 ))
  (dir-locals-set-directory-class (file-truename dir) 'read-only))

(let ((f "~/Dropbox/.config/emacs/config.el"))
  (when (file-exists-p f)
    (load-file f)))

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'magit-diff-edit-hunk-commit 'disabled nil)
