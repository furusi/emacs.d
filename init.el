;;; init.el --- my init script  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(show-paren-mode t)
(column-number-mode)

(defvar my-dropbox-dir  (expand-file-name "~/Dropbox"))
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
  (when (file-exists-p (locate-user-emacs-file "custom.el"))
    (load-file (locate-user-emacs-file "custom.el")))
  )

(leaf custom-variables
  :doc "set custom variables"
  :custom
  `((auth-sources . '(,(locate-user-emacs-file "authinfo.gpg")))
    (auto-save-interval . 10)
    (backup-directory-alist . '((".*" . "~/.ehist")))
    (byte-compile-warnings . '(cl-functions))
    (comment-style . 'multi-line)
    (custom-theme-directory . ,(locate-user-emacs-file "themes")) ;; テーマのディレクトリを設定
    (cursor-type . '(bar . 4))
    (default-frame-alist .'((width . 180) (height . 40)))
    (ediff-diff-options . "-w")
    (ediff-split-window-function . 'split-window-horizontally)
    (ediff-window-setup-function . 'ediff-setup-windows-plain)
    ;; (garbage-collection-messages . t) ; GC発動のタイミングを確認するときに有効にする
    (eol-mnemonic-dos . "(CRLF)")
    (eol-mnemonic-mac . "(CR)")
    (eol-mnemonic-unix . "(LF)")
    (indent-tabs-mode . nil)
    (inhibit-startup-screen . t)
    (mark-ring-max . 32);; マークの数を32に増やす
    (menu-bar-mode . t)
    (package-user-dir . ,(locate-user-emacs-file (format "elpa/%s" emacs-version)))
    (recentf-auto-cleanup . 'never)
    (recentf-max-menu-items . 30)
    (recentf-max-saved-items . 2000)
    (set-mark-command-repeat-pop . t)    ;; C-u C-SPCの後C-SPCだけでマークを遡れる
    (show-paren-style . 'mixed)
    (tramp-ssh-controlmaster-options . "-4") ; ssh接続時にipv4アドレスを利用する
    (tool-bar-mode . nil)
    ;; (truncate-lines . t)         ;文字列を折り返さない
    (use-dialog-box . nil)
    (use-file-dialog . nil)
    (vc-follow-symlinks . t)
    (vc-handled-backends . '(Git))
    ))

(leaf yes-or-no
  :emacs>= 28.1
  :custom
  (use-short-answers . t))

(leaf emacs29
  :emacs>= 29
  :config
  (when window-system
    (pixel-scroll-precision-mode)))

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
  (when (eq system-type 'gnu/linux)
    (cond
     ;; wsl
     ((string-match ".*-microsoft-standard-WSL2.*"
                    operating-system-release)
      (setq
       browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
       browse-url-generic-args     '("/c" "start")
       browse-url-browser-function #'browse-url-generic))))
  (when (eq system-type 'darwin)
    (setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
  )

(leaf recentf
  :custom `(recentf-save-file . ,(locate-user-emacs-file (format "recentf-%s" emacs-version))))

(leaf xref
  :defvar auto-read-only-dirs
  :hook (xref-after-jump-hook .
                              (lambda ()
                                (dolist (f auto-read-only-dirs)
                                  (when (string-match-p (expand-file-name f) buffer-file-name)
                                    (view-mode))))
                              )
  :config
  (defvar auto-read-only-dirs
    '("/opt/homebrew/Cellar/"
      "~/.cargo/registry/"
      "~/.emacs.d/packages/"
      "~/.rustup/toolchains/")))
(leaf view-mode
  :bind
  (:view-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ;; ("SPC". scroll-up-command)
   ;; ("S-SPC". scroll-down-command)
   ))

(leaf deepl-translate
  :url "https://uwabami.github.io/cc-env/Emacs.html"
  :commands my-deepl-translate
  :bind
  (:embark-region-map
   :package embark
   ("T" . my-deepl-translate))
  :preface
  (require 'url-util)
  (defun my-deepl-translate (&optional string)
    (interactive)
    (setq string
          (cond ((stringp string) string)
                ((use-region-p)
                 (string-fill (buffer-substring (region-beginning) (region-end))
                              5000))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (run-at-time 0.1 nil 'deactivate-mark)
    (let* ((string (replace-regexp-in-string
                    "|" (regexp-quote "\x005c\x007c")
                    (replace-regexp-in-string
                     "/"
                     (regexp-quote "\x005c\x002f")
                     string))
                   )
           (url (format "https://www.deepl.com/translator#en/ja/%s"
                        (url-hexify-string string))))
      (cond ((eq system-type 'darwin)
             (browse-url-default-macosx-browser url))
            ((string-match ".*-microsoft-standard-WSL2.*" operating-system-release)
             (browse-url-generic url))
            (t
             (browse-url-firefox url)))))
  )

(leaf image-mode
  :bind (:image-mode-map
         ("=" . image-increase-size)))

(leaf help-mode
  :bind
  (:help-mode-map
   ("n" . next-line)
   ("j" . next-line)
   ("p" . previous-line)
   ("k" . previous-line)
   ("v" . scroll-up-command)
   ("V" . scroll-down-command)))
(elpaca helpful
  (leaf helpful
    :doc "A better *help* buffer"
    :req "emacs-25" "dash-2.18.0" "s-1.11.0" "f-0.20.0" "elisp-refs-1.2"
    :tag "lisp" "help" "emacs>=25"
    :url "https://github.com/Wilfred/helpful"
    :emacs>= 25
    :bind
    ((:help-map
      :package help
      ("v" . helpful-variable)
      ("f" . helpful-callable)
      ("o" . helpful-symbol)
      ("k" . helpful-key)
      )
     (:embark-symbol-map
      :package embark
      ("h" . helpful-symbol)))
    )
  )
(leaf diff-mode
  :bind
  (:diff-mode-map
   ("v" . scroll-up-command)
   ("V" . scroll-down-command))
  :hook
  (diff-mode-hook . (lambda () (read-only-mode t)))
  )

(leaf autorevert
  :hook
  (emacs-startup-hook . global-auto-revert-mode)
  )

(leaf window
  :emacs>= 28
  :bind
  (:resize-window-repeat-map
   ("+" . enlarge-window)
   ("=" . enlarge-window)
   ("-" . shrink-window)
   ("_" . shrink-window)
   (">" . enlarge-window-horizontally)
   ("<" . shrink-window-horizontally))
  )

(elpaca (initchart :host github :repo "yuttie/initchart")
  (leaf initchart
    :disabled t
    :require t
    :config
    (initchart-record-execution-time-of load file)
    (initchart-record-execution-time-of require feature))
  )

(defun which-linux-distribution ()
  "Return string which obtains from 'lsb_release' command."
  (interactive)
  (if (eq system-type 'gnu/linux)
      (string-trim (shell-command-to-string "lsb_release -sd")
                   "^\"" "\"?[ \t\n\r]+")
    ""))
(setq my-lsb-distribution-name
      (which-linux-distribution))

(recentf-mode 1)

;;行番号を表示
(if (version<= "26.0.50" emacs-version)
    (progn
      ;; (global-display-line-numbers-mode)
      (setq-default indicate-empty-lines t)
      (setq-default indicate-buffer-boundaries 'left)))
(elpaca exec-path-from-shell
  (leaf exec-path-from-shell
    :require t
    :config
    (add-to-list 'exec-path-from-shell-variables "PYTHONPATH")
    (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
    (exec-path-from-shell-initialize)))
(elpaca system-packages
  (leaf system-packages
    :require t
    :config
    (when (eq system-type 'darwin)
      (setq system-packages-use-sudo nil
            system-packages-package-manager 'brew))
    (when (or (string-match-p "arch" operating-system-release)
              (string-match-p "manjaro" operating-system-release)
              (string-match-p "endeavouros" my-lsb-distribution-name))
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
  )

(leaf bind-key
  :bind
  (("M-<f1>" . other-frame)  ;Macのショートカットに合わせる
   ;; ("C-o" . my-insert-newline-and-indent)
   (:isearch-mode-map
    ("C-o" . isearch-exit))
   (:reb-mode-map
    :package re-builder
    ("C-c C-k". reb-quit))))

(leaf special-characer-mode
  :url "https://github.com/madanh/special-characer-mode"
  :config
  (defmacro ins-val (val)
    `(lambda () (interactive) (insert ,val)))
  (define-minor-mode special-char-mode
    "Toggle Special Character mode"
    :init-value " SpecialChar"
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
      (,(kbd "0") . ,(ins-val ")")) (,(kbd ")") . ,(ins-val "0")) (,[kp-0] . ,(ins-val "0")))
    ))

(when (equal system-type 'darwin)
  (setq ns-command-modifier 'meta)
  (when (memq window-system '(ns mac))
    ;; 游教科書体
    ;; (set-face-attribute 'default nil
    ;;                     :family "YuKyokasho Yoko")
    ;; 源ノ角ゴシック
    (set-face-attribute 'default nil
                        :family "UDEV Gothic JPDOC")
    (set-fontset-font nil '(#x30000 . #x3134F) (font-spec :family "Source Han Sans SC"))
    (set-fontset-font nil '(#xAA80 . #xAADF) (font-spec :family "Noto Sans Tai Viet"))
    (let* ((variable-tuple
            (cond ((x-list-fonts "UDEV Gothic JPDOC") '(:font "UDEV Gothic JPDOC"))
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
  (add-to-list 'load-path "~/opt/mu-1.0/mu4e")
  ;;曖昧な文字幅を指定する
  (aset char-width-table ?→ 2)

  (when (eq window-system 'x)
    (set-face-attribute 'default nil :family "UDEV Gothic JPDOC")))

;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
(setq use-default-font-for-symbols nil)

(elpaca restart-emacs)

(leaf dired
  :custom
  ((dired-dwim-target . t)
   (dired-recursive-copies . 'always)
   (dired-listing-switches . "-alFh"))
  :config
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil)))

(leaf xwidget
  :leaf-autoload nil
  :leaf-defun nil
  :bind
  (:xwidget-webkit-mode-map
   ("j" . xwidget-webkit-scroll-up-line)
   ("k" . xwidget-webkit-scroll-down-line))
  :hook
  ((xwidget-webkit-mode-hook . (lambda ()
                                 (display-line-numbers-mode -1)))))
(elpaca pomodoro
  (leaf pomodoro
    :doc "A timer for the Pomodoro Technique"
    :require t
    :config
    (when (eq window-system 'ns)
      (setq pomodoro-sound-player "afplay"))
    
    (let ((sound (cond
                  ((or (string-match "Ubuntu" my-lsb-distribution-name)
                       (string-match "debian" my-lsb-distribution-name))
                   "/usr/share/sounds/gnome/default/alerts/glass.ogg")
                  ((string-match "endeavouros" my-lsb-distribution-name)
                   "/usr/share/sounds/freedesktop/stereo/service-login.oga")
                  ((eq window-system 'ns)
                   "/System/Library/Sounds/Glass.aiff"))))
      (setq pomodoro-work-start-sound sound
            pomodoro-break-start-sound sound))
    (pomodoro-add-to-mode-line))
  )
(elpaca sudo-edit)
(elpaca so-long)
(elpaca japanese-holidays
  (leaf japanese-holidays
    :doc "Calendar functions for the Japanese calendar"
    :req "emacs-24.1" "cl-lib-0.3"
    :tag "calendar" "emacs>=24.1"
    :url "https://github.com/emacs-jp/japanese-holidays"
    :emacs>= 24.1
    :require t
    :config
    (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
          (append japanese-holidays holiday-local-holidays holiday-other-holidays))
    (setq calendar-mark-holidays-flag t)	; 祝日をカレンダーに表示
    ;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
    ;; デフォルトで設定済み
    (setq japanese-holiday-weekend '(0 6)	   ; 土日を祝日として表示
          japanese-holiday-weekend-marker	   ; 土曜日を水色で表示
          '(holiday nil nil nil nil nil japanese-holiday-saturday))
    (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend))
  )
(elpaca magit
  (leaf magit
    :require t
    :bind (("C-x g" . magit-status)
           (:magit-diff-mode-map
            ("=" . magit-diff-more-context)))
    :hook
    (ediff-keymap-setup-hook . add-d-to-ediff-mode-map)
    :custom
    ((magit-display-buffer-function . 'magit-display-buffer-fullframe-status-v1)
     (magit-diff-refine-hunk . 'all))
    :init
    (defun my-magit-mode-bury-buffer ()
      (interactive)
      (call-interactively #'magit-mode-bury-buffer)
      (when (< 1(length (tab-bar-tabs)))
        (tab-close))
      )
    ;; https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750
    (defun ediff-copy-both-to-C ()
      (interactive)
      (ediff-copy-diff ediff-current-difference nil 'C nil
                       (concat
                        (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                        (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
    (defun add-d-to-ediff-mode-map ()
      (when (ediff-merge-job)
        (define-key ediff-mode-map "d" 'ediff-copy-both-to-C)
        (setq-local ediff-long-help-message-merge
                    "
p,DEL -previous diff  |     | -vert/horiz split   |  x -copy buf X's region to C
n,SPC -next diff      |     h -highlighting       |  d -copy both to C
    j -jump to diff   |     @ -auto-refinement    |  r -restore buf C's old diff
   gx -goto X's point |    ## -ignore whitespace  |  * -refine current region
  C-l -recenter       | #f/#h -focus/hide regions |  ! -update diff regions
  v/V -scroll up/dn   |     X -read-only in buf X |  + -combine diff regions
  </> -scroll lt/rt   |     m -wide display       | wx -save buf X
    ~ -swap variants  |     s -shrink window C    | wd -save diff output
                      |  $$ -show clashes only    |  / -show/hide ancestor buff
                      |  $* -skip changed regions |  & -merge w/new default
"
                    )))
    :config
    (require 'magit-extras)
    ;; ediff時にorgファイルを全て表示する
    (defun my-ediff-prepare-buffer-function ()
      (org-show-all))
    (add-hook 'ediff-prepare-buffer-hook #'my-ediff-prepare-buffer-function)
    )
  )
(elpaca magit-svn)
(elpaca blamer
  (leaf blamer
    :doc "Show git blame info about current line"
    :req "emacs-27.1" "posframe-1.1.7"
    :tag "emacs>=27.1"
    :url "https://github.com/artawower/blamer.el"
    :emacs>= 27.1
    ))
(elpaca projectile
  (leaf projectile
    :require t
    :bind `((:projectile-mode-map
             ("C-c p" . projectile-command-map))
            ,(when (string> emacs-version "28")
               '(:projectile-command-map
                 ("v" . my-projectile-vc-in-new-tab))))
    :custom
    `((projectile-cache-file . ,(locate-user-emacs-file
                                 (format "projectile/%s/projectile.cache" emacs-version)))
      (projectile-known-projects-file . ,(locate-user-emacs-file
                                          (format "projectile/%s/projectile-bookmarks.eld" emacs-version)))
      (projectile-sort-order . 'recently-active)
      (projectile-switch-project-action . 'projectile-commander))
    :init
    (let ((dir (locate-user-emacs-file (format "projectile/%s" emacs-version))))
      (unless (file-directory-p dir)
        (make-directory dir t))
      )
    (defun my-projectile-vc-in-new-tab ()
      (interactive)
      (let ((tab-name-list (mapcar #'cdadr (tab-bar-tabs)))
            (tab-name (format "=p:%s"
                              (replace-regexp-in-string
                               "\.emacs\.d/packages/.*/.*repos" "REPO"
                               (replace-regexp-in-string
                                (format "^%s" (getenv "HOME")) "~"
                                (projectile-acquire-root)))))
            (project-root (projectile-acquire-root)))
        (cond
         ;; 既に同名のタブがあったらそれを使う
         ((member tab-name tab-name-list)
          (tab-switch tab-name)
          (projectile-vc project-root))
         ((not (memq major-mode '(magit-diff-mode
                                  magit-log-mode
                                  magit-revision-mode
                                  magit-status-mode)))
          (other-tab-prefix)
          (projectile-vc)
          (tab-rename tab-name))
         (t
          (projectile-vc))))
      )
    :config
    (projectile-mode +1)
    (dolist
        (d '(".ccls-cache"))
      (add-to-list 'projectile-globally-ignored-directories d))
    (when (string> emacs-version "28")
      (def-projectile-commander-method ?v "Open project root in vc-dir or magit."
        (my-projectile-vc-in-new-tab)))
    )
  )
(leaf projectile-for-eglot
  :url "https://glassonion.hatenablog.com/entry/2019/05/11/134135"
  :after projectile
  :preface
  (defun my-projectile-project-find-function (dir)
    (let ((root (projectile-project-root dir)))
      (and root
           (cons 'transient root))))
  :config
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions #'my-projectile-project-find-function)))
;; ddskk
(elpaca (ddskk :host github :repo "skk-dev/ddskk" :depth 10)
  (leaf ddskk
    :commands skk-mode
    :bind (("C-x C-j" . skk-mode)
           (:minibuffer-local-map
            ("C-j" . skk-kakutei)))
    :hook ((skk-load-hook . (lambda () (require 'context-skk))) ;自動的に英字モードになる
           (skk-jisyo-edit-mode-hook . (lambda () (read-only-mode t)))
           )
    :custom
    `((default-input-method . "japanese-skk")
      (skk-auto-insert-paren . t)
      (skk-dcomp-activate . t)         ;動的補完
      (skk-delete-implies-kakutei . nil) ; ▼モードで BS を押したときには確定しないで前候補を表示する
      (skk-egg-like-newline . t)           ;non-nilにするとEnterでの確定時に改行しない
      (skk-get-jisyo-directory . ,(expand-file-name (locate-user-emacs-file "skk-get-jisyo")))
      (skk-henkan-show-candidates-keys . '(?a ?o ?e ?u ?h ?t ?n ?s))
      (skk-henkan-strict-okuri-precedence . t)
      (skk-isearch-start-mode . 'latin); isearch で skk の初期状態
      (skk-kutouten-type . 'en)
      (skk-save-jisyo-instantly . t)
      (skk-search-katakana . 'jisx0201-kana)
      (skk-search-sagyo-henkaku . t)   ;サ行変格活用の動詞も送りあり変換出来るようにする
      (skk-share-private-jisyo . t)
      (skk-sticky-key . '(117 101))
      (skk-use-act . t)                ;全角・半角カタカナを変換候補にする
      (skk-use-jisx0201-input-method . t)
      (skk-user-directory . ,(locate-user-emacs-file "ddskk"))
      (skk-japanese-message-and-error . t)
      )
    :init
    (leaf skk-dropbox
      :if (file-exists-p "~/Dropbox/.config/ddskk")
      :custom
      ((skk-jisyo . "~/Dropbox/.config/ddskk/jisyo")
       (skk-jisyo-code . 'utf-8)))
    
    (let ((skk-jisyo-directory
           (if (file-exists-p "~/Dropbox/.config/ddskk/skkdic-utf8")
               "~/Dropbox/.config/ddskk/skkdic-utf8"
             skk-get-jisyo-directory)))
      (setq skk-large-jisyo (format "%s/SKK-JISYO.L" skk-jisyo-directory))
      (setq skk-extra-jisyo-file-list
            (mapcar (lambda (x)
                      (format "%s/%s" skk-jisyo-directory x))
                    '("SKK-JISYO.lisp" "SKK-JISYO.station"
                      "SKK-JISYO.assoc" "SKK-JISYO.edict"
                      "SKK-JISYO.law" "SKK-JISYO.jinmei"
                      "SKK-JISYO.fullname" "SKK-JISYO.geo"
                      "SKK-JISYO.itaiji" "SKK-JISYO.zipcode"
                      "SKK-JISYO.okinawa" "SKK-JISYO.propernoun"))))
    (with-eval-after-load 'dired
      (load "dired-x")
      (global-set-key "\C-x\C-j" 'skk-mode))
    (leaf skk-study
      :require t)
    (leaf skk-hint
      :require t
      :custom
      (skk-hint-start-char . ?=)          ;▼モード中で=漢字の読み方を指定する
      )
    (leaf context-skk
      :config
      (dolist (mode '(python-mode js-mode rustic-mode dart-mode))
        (add-to-list 'context-skk-programming-mode mode))
      (setq context-skk-mode-off-message "[context-skk] 日本語入力 off")
      (defun my-context-skk-at-heading-p ()
        (if (bolp)
            (cond
             ((org-at-heading-p) t)
             ((or (org-at-item-bullet-p) (org-at-item-checkbox-p)) t)
             ((org-at-block-p) t)
             (t nil))
          nil))
      (add-hook 'org-mode-hook
                (lambda ()
                  (setq-local
                   context-skk-context-check-hook
                   '(my-context-skk-at-heading-p
                     context-skk-in-read-only-p))))
      (context-skk-mode)
      )
    (defun skk-set-display-table ()
      (walk-windows (lambda (w)
                      (let ((disptab (make-display-table)))
                        (aset disptab ?\▼ (vector (make-glyph-code ?# 'escape-glyph)))
                        (aset disptab ?\▽ (vector (make-glyph-code ?@ 'escape-glyph)))
                        (set-window-display-table w disptab)))))
    (require 'ccc)
    (add-hook 'window-configuration-change-hook #'skk-set-display-table)
    (add-hook 'after-init-hook #'skk-set-display-table))
  )
(leaf eww
  :commands (eww)
  :custom
  (eww-search-prefix . "https://www.google.co.jp/search?q=")
  :config
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
    (setq-local shr-put-image-function 'shr-put-image-alt)))

(leaf *vertico
  :config
  (elpaca (vertico :host github :repo "minad/vertico" :files (:defaults "extensions/*.el"))
    (leaf vertico
      :emacs>= 27.1
      :require t
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
      :config
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
      :require t
      :bind (("C-x c r" . vertico-repeat-last)
             ("C-x c R" . vertico-repeat-select))
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
    )
  ;; Use the `orderless' completion style.
  ;; Enable `partial-completion' for files to allow path expansion.
  ;; You may prefer to use `initials' instead of `partial-completion'.
  (elpaca orderless
    (leaf orderless
      :init
      (setq completion-styles '(orderless basic)
            completion-category-defaults nil
            completion-category-overrides '((file (styles basic partial-completion))))
      ))
  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (leaf savehist
    :init
    (savehist-mode))
  (elpaca (consult :host github :repo "minad/consult")
    (leaf consult
      :require consult consult-xref consult-org consult-imenu consult-register
      :custom
      ((consult-async-min-input . 2)
       (consult-narrow-key . ">")
       (consult-ripgrep-args
        . "rg --hidden --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")
       (xref-show-definitions-function . #'consult-xref)
       (xref-show-xrefs-function . #'consult-xref))
      :bind (("C-c h" . consult-history)
             ("C-c m" . consult-mode-command)
             ("C-x C-SPC" . consult-global-mark)
             ("C-x b" . consult-buffer)
             ("C-x c i" . consult-imenu)
             ("C-x j" . consult-recent-file)
             ("C-x r j" . consult-register)
             ("C-x r l"  . consult-bookmark)
             ("M-y" . consult-yank-pop)
             ("C-x 4 b" . consult-buffer-other-window)
             ("C-x 5 b" . consult-buffer-other-frame)
             ("C-x r SPC" . consult-register-store)
             ("C-h i" . consult-info)
             ([remap goto-line] . consult-goto-line)
             (:isearch-mode-map
              ("C-i" . my-consult-line)
              ("M-e" . consult-isearch-history))
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
                                            arg 'extended t)))
          (when re
      (cons (append
                            (list consult--fd-command
                                  "--color=never" "--full-path"
                                  (consult--join-regexps re 'extended))
                            opts)
            hl))))

      (defun consult-fd (&optional dir initial)
        "Search with fd for files in DIR matching input regexp given INITIAL input."
        (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
               (default-directory dir))
    (find-file (consult--find prompt #'consult--fd-builder initial))))
      (consult-customize
       consult-theme
       :preview-key (list :debounce 1.0 'any)
       consult-goto-line consult-line
       :preview-key (list 'any)
       consult-ripgrep consult-git-grep consult-grep
       consult-bookmark consult-recent-file consult-xref
       consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
       consult-find consult-fd consult-org-agenda
       :preview-key (if window-system "C-," "M-,"))
      (autoload 'projectile-project-root "projectile")
      (setq consult-project-function #'projectile-project-root)
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
    )
  (elpaca consult-yasnippet
    (leaf consult-yasnippet
      :doc "A consulting-read interface for yasnippet"
      :req "emacs-27.1" "yasnippet-0.14" "consult-0.9"
      :tag "emacs>=27.1"
      :url "https://github.com/mohkale/consult-yasnippet"
      :emacs>= 27.1
      :after yasnippet consult))
  (elpaca consult-projectile)
  (elpaca affe
    (leaf affe
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
      (consult-customize affe-grep :preview-key "M-.")))
  (elpaca marginalia
    (leaf marginalia
      :bind (
             ("M-A" . marginalia-cycle)
             (:minibuffer-local-map
              ("M-A" . marginalia-cycle))
             )
      :init
      (marginalia-mode)))
  (elpaca (embark :files (:defaults ("embark-org.el")))
    (leaf embark
      :emacs>= 26.1
      :require t
      :bind
      `((,(if window-system "C-." "M-.") . embark-act)         ;; pick some comfortable binding
        ("C-;" . embark-dwim)        ;; good alternative: M-.
        ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
        (:embark-package-map
         ("b" . embark-browse-package-url))
        (:embark-region-map
         ("C-l" . my-lookup-mkdict))
        (:embark-symbol-map
         ("C-l" . my-lookup-mkdict)))
      :init
      ;; Optionally replace the key help with a completing-read interface
      ;; (setq prefix-help-command #'embark-prefix-help-command)
      (defun my-lookup-mkdict ()
        (interactive)
        (let ((str (read-from-minibuffer "Input: ")))
          (call-process
           "open" nil 0 nil
           (concat "mkdictionaries:///?text=" str)))
        )
      :config
      ;; Hide the mode line of the Embark live/completions buffers
      (add-to-list 'display-buffer-alist
                   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                     nil
                     (window-parameters (mode-line-format . none))))
      ))
  ;; Consult users will also want the embark-consult package.
  (elpaca embark-consult
    (leaf embark-consult
      :after embark
      :require t
      :hook
      (embark-collect-mode-hook . consult-preview-at-point-mode)
      ;; :init (with-eval-after-load 'embark
      ;;         (require 'embark-consult))
      ))
  (elpaca all-the-icons-completion
    (leaf all-the-icons-completion
      :doc "Add icons to completion candidates"
      :req "emacs-26.1" "all-the-icons-5.0"
      :tag "lisp" "convenient" "emacs>=26.1"
      :url "https://github.com/iyefrat/all-the-icons-completion"
      :emacs>= 26.1
      :after all-the-icons
      :config
      (all-the-icons-completion-mode t)))
  (elpaca (corfu :host github :repo "minad/corfu" :depth 10 :files (:defaults "extensions/*.el"))
    (leaf corfu
      :url "https://github.com/minad/corfu"
      :emacs>= 27.1
      :require (corfu corfu-popupinfo)
      :bind
      (:corfu-map
       ("M-SPC" . corfu-insert-separator)
       ("M-m" . corfu-move-to-minibuffer)
       )
      :custom
      ((completion-cycle-threshold . 3)
       (corfu-auto . t)
       (corfu-cycle . t)
       (corfu-exclude-modes . '(rustic-mode rust-mode)))
      :hook
      (eshell-mode-hook . (lambda ()
                            (setq-local corfu-auto t
                                        corfu-quit-no-match 'separator)))
      :init
      (defun corfu-move-to-minibuffer ()
        (interactive)
        (let ((completion-extra-properties corfu--extra)
              completion-cycle-threshold completion-cycling)
          (apply #'consult-completion-in-region completion-in-region--data)))
      :config
      (global-corfu-mode)
      (corfu-popupinfo-mode t)
      ))
  
  (elpaca popon
    (leaf popon
      :init
      (unless (display-graphic-p)
        (require 'popon))))
  (elpaca corfu-terminal
    (leaf corfu-terminal
      :after corfu popon
      :require t
      :config
      (unless (display-graphic-p)
        (corfu-terminal-mode +1))
      ))
  (elpaca tempel
    (leaf tempel
      :doc "Tempo templates/snippets with in-buffer field editing"
      :req "emacs-27.1"
      :tag "emacs>=27.1"
      :url "https://github.com/minad/tempel"
      :emacs>= 27.1
      :require t
      :bind
      ((("M-+" . tempel-complete) ;; Alternative tempel-expand
        ("M-*" . tempel-insert))
       (:tempel-map
        ("C-i" . tempel-next)
        ))
      :custom
      `((tempel-path . ,(format "%ssnippets/tempel/templates/*" user-emacs-directory)))
      :config
      (defun tempel-setup-capf ()
        ;; Add the Tempel Capf to `completion-at-point-functions'. `tempel-expand'
        ;; only triggers on exact matches. Alternatively use `tempel-complete' if
        ;; you want to see all matches, but then Tempel will probably trigger too
        ;; often when you don't expect it.
        ;; NOTE: We add `tempel-expand' *before* the main programming mode Capf,
        ;; such that it will be tried first.
        (setq-local completion-at-point-functions
                    (cons #'tempel-expand
                          completion-at-point-functions)))
      (add-hook 'prog-mode-hook 'tempel-setup-capf)
      (add-hook 'text-mode-hook 'tempel-setup-capf)
      ))
  (elpaca cape
    (leaf cape
      :doc "Completion At Point Extensions"
      :req "emacs-27.1"
      :tag "emacs>=27.1"
      :url "https://github.com/minad/cape"
      :emacs>= 27.1
      :bind
      (("C-c f p" . completion-at-point) ;; capf
       ("C-c f t" . complete-tag)        ;; etags
       ("C-c f d" . cape-dabbrev)        ;; or dabbrev-completion
       ("C-c f h" . cape-history)
       ("C-c f f" . cape-file)
       ("C-c f k" . cape-keyword)
       ("C-c f s" . cape-symbol)
       ("C-c f a" . cape-abbrev)
       ("C-c f i" . cape-ispell)
       ("C-c f l" . cape-line)
       ("C-c f w" . cape-dict)
       ("C-c f \\" . cape-tex)
       ("C-c f _" . cape-tex)
       ("C-c f ^" . cape-tex)
       ("C-c f &" . cape-sgml)
       ("C-c f r" . cape-rfc1345))
      :init
      (add-to-list 'completion-at-point-functions #'cape-dabbrev)
      (add-to-list 'completion-at-point-functions #'cape-keyword)
      (add-to-list 'completion-at-point-functions #'cape-tex)
      (add-to-list 'completion-at-point-functions #'cape-file)
      :config
      (if (memq system-type '(darwin gnu/linux))
          (customize-set-variable 'cape-dict-file "/usr/share/dict/words"))))
  (elpaca (kind-icon :host github :repo "jdtsmith/kind-icon")
    (leaf kind-icon
      :emacs>= 27.1
      :require t
      :after corfu
      :custom
      (kind-icon-default-face . 'corfu-default)
      :config
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
      ))
  )
(elpaca migemo
  (leaf migemo
    :unless (equal (shell-command-to-string "command -v cmigemo") "")
    :require t
    :custom
    (migemo-options . '("-q" "--emacs"))
    (migemo-coding-system . 'utf-8-unix)
    (migemo-user-dictionary . nil)
    (migemo-regex-dictionary . nil)
    :config
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
    (load-library "migemo")
    ;; https://www.yewton.net/2022/02/07/consult-ripgrep-migemo/
    (defun consult--migemo-regexp-compiler (input type ignore-case)
      (setq input (mapcar #'migemo-get-pattern (consult--split-escaped input)))
      (cons (mapcar (lambda (x) (consult--convert-regexp x type)) input)
            (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
              (apply-partially #'consult--highlight-regexps regexps ignore-case))))
    (setq migemo-options '("--quiet" "--nonewline" "--emacs"))
    (setq consult--regexp-compiler #'consult--migemo-regexp-compiler)
    (migemo-init))
  )
;; SLIMEのロード
(elpaca undo-tree
  (leaf undo-tree
    :diminish (global-undo-tree-mode undo-tree-mode)
    :require t
    :global-minor-mode global-undo-tree-mode
    :custom
    ((undo-tree-history-directory-alist . '(("." . "~/.emacs.d/undo-tree")))
     (undo-tree-incompatible-major-modes . '(term-mode fundamental-mode))
     (undo-tree-visualizer-diff . t)))
  )
(elpaca undo-fu)
(elpaca undo-fu-session
  (leaf undo-fu-session
    :doc "Persistent undo, available between sessions"
    :req "emacs-28.1"
    :tag "convenience" "emacs>=28.1"
    :url "https://codeberg.org/ideasman42/emacs-undo-fu-session"
    :emacs>= 28.1
    :require t
    :custom
    (undo-fu-session-incompatible-files . '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
    :config
    (global-undo-fu-session-mode))
  )
(elpaca rust-mode
  (leaf rust-mode
    :doc "A major-mode for editing Rust source code"
    :req "emacs-25.1"
    :tag "languages" "emacs>=25.1"
    :url "https://github.com/rust-lang/rust-mode"
    :emacs>= 25.1
    :hook (rust-mode-hook . (lambda () (prettify-symbols-mode)))
    :config
    (push '(".add" . ?∔) rust-prettify-symbols-alist)
    )
  )
(elpaca rustic
  (leaf rustic
    :doc "Rust development environment"
    :req "emacs-26.1" "rust-mode-1.0.3" "dash-2.13.0" "f-0.18.2"
    "let-alist-1.0.4" "markdown-mode-2.3" "project-0.3.0" "s-1.10.0"
    "seq-2.3" "spinner-1.7.3" "xterm-color-1.6"
    :tag "languages" "emacs>=26.1"
    :emacs>= 26.1
    :require rustic
    ;; :custom `((rustic-lsp-client . 'lsp-mode))
    :hook
    (rustic-mode-hook . (lambda ()
                          (electric-pair-mode 1)
                          (if (and (eq rustic-lsp-client 'nil) (featurep 'lsp-bridge))
                              (lsp-bridge-mode)
                            (corfu-mode))
                          (when (featurep 'embark)
                            (setq-local embark-target-finders
                                        (append (remove
                                                 'embark-target-file-at-point
                                                 embark-target-finders)
                                                '(embark-target-file-at-point)))
                            )))
    :config
    ;; (when (eq rustic-lsp-client 'eglot)
    ;;   (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))))
    (leaf rustic-color
      :after modus-themes
      :custom (rustic-ansi-faces . ["black" "red3" "green3" "yellow3"
      "deep sky blue" "magenta3" "cyan3" "white"]))
    )
  )
(elpaca lsp-haskell
  (leaf lsp-haskell
    :doc "Haskell support for lsp-mode"
    :req "emacs-24.3" "lsp-mode-3.0"
    :tag "haskell" "emacs>=24.3"
    :url "https://github.com/emacs-lsp/lsp-haskell"
    :emacs>= 24.3
    :after lsp-mode
    :require t
    :hook (haskell-mode-hook . lsp))
  )
(elpaca ron-mode
  (leaf ron-mode
    :doc "Rusty Object Notation mode"
    :req "emacs-24.5.1"
    :tag "languages" "emacs>=24.5.1"
    :url "https://chiselapp.com/user/Hutzdog/repository/ron-mode/home"
    :emacs>= 24.5
    :mode (("\\.ron$" . ron-mode)))
  )
(elpaca moody
  (leaf moody
    :doc "Tabs and ribbons for the mode line"
    :req "emacs-25.3" "compat-28.1.1.0"
    :url "https://github.com/tarsius/moody"
    :custom
    ((x-underline-at-descent-line . t))
    :config
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode))
  )
(leaf dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))
(elpaca wgrep
  (leaf wgrep
    :require t
    :custom
    ((wgrep-enable-key . "e")
     (wgrep-auto-save-buffer . t))
    )
  )
(elpaca highlight-symbol)
(elpaca expand-region)
(leaf expand-region
  :bind (("C-=" . er/expand-region)))
(when (display-graphic-p)
  (elpaca all-the-icons)
  )
(elpaca which-key
  (leaf which-key
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
  )
;;;yasnippet
(elpaca yasnippet
  (leaf yasnippet
    :require t
    :diminish yas-minor-mode
    :config
    (yas-global-mode 1)
    (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets/yasnippet"))
    )
  )
(elpaca yasnippet-snippets
  (leaf yasnippet-snippets
    :after yasnippet)
  )
(elpaca gitignore-templates)
(leaf rst
  :bind ((:rst-mode-map
          ("M-RET" . rst-insert-list)))
  :config
  (when (eq system-type 'darwin)
    (setq rst-pdf-program "open -a Skim")
    (setq rst-slides-program "open -a Firefox")))
(elpaca gradle-mode
  (leaf gradle-mode
    :require t
    :mode (("\\.gradle$" . gradle-mode)))
  )
(elpaca slime
  (leaf slime
    :if (file-exists-p "~/.roswell/helper.el")
    :custom
    ((slime-auto-start . 'ask)
     )
    :hook ((lisp-mode-hook . slime-mode)
           )
    :config
    ;; (slime-setup '(slime-fancy slime-company))
    
    
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
  )
(elpaca slime-company
  (leaf slime-company
    :after slime
    :custom ((slime-company-completion . 'fuzzy)
             (slime-complete-symbol*-fancy . t))
    :hook ((slime-repl-mode-hook
            . (lambda () (add-to-list
                          'company-backends
                          '(company-slime company-dabbrev-code)))))))
(elpaca web-mode
  (leaf web-mode
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
  )
;; Org-mode
(leaf org*
  :config
  (leaf org
    :mode (("\\.org$" . org-mode))
    :hook ((org-mode-hook . (lambda () (prettify-symbols-mode)))
           (org-mode-hook . (lambda () (setq prettify-symbols-alist org-prettify-symbols-alist))))
      :custom
      ((org-export-allow-bind-keywords . t)
       (org-babel-python-command . "python3")
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
       (org-special-ctrl-a/e . t)
       ;; (org-src-lang-modes . '(("C" . c)
       ;;                         ("C++" . c++)
       ;;                         ("arduino" . arduino)
       ;;                         ("asm" . asm)
       ;;                         ("asymptote" . asy)
       ;;                         ("bash" . sh)
       ;;                         ("browser" . html)
       ;;                         ("browser" . web)
       ;;                         ("calc" . fundamental)
       ;;                         ("cpp" . c++)
       ;;                         ("ditaa" . artist)
       ;;                         ("dot" . graphviz-dot)
       ;;                         ("elisp" . emacs-lisp)
       ;;                         ("html" . web)
       ;;                         ("ocaml" . tuareg)
       ;;                         ("php" . php)
       ;;                         ("python" . python)
       ;;                         ("redis" . redis)
       ;;                         ("rust" . rustic)
       ;;                         ("screen" . shell-script)
       ;;                         ("shell" . sh)
       ;;                         ("sqlite" . sql)
       ;;                         ))
       (org-src-preserve-indentation . t)
       (org-startup-folded . t)
       (org-preview-latex-default-process . 'dvisvgm)
       (org-clock-persist . t)
       (org-enforce-todo-dependencies . t)
       (org-enforce-todo-checkbox-dependencies . t)
       (org-use-sub-superscripts . '{})
       (org-export-with-sub-superscripts . '{})
       )
      :bind (("C-c c" . org-capture)
             ("C-c l" . org-store-link)
             ("C-c a" . org-agenda)
             ("<f2>" . insert-zero-width-space)
             (:org-mode-map
              ("C-c C-\'" . org-insert-structure-template)))
      :init
      (defun my-org-item-speed-command-help ()
        (interactive)
        (with-output-to-temp-buffer "*Help*"
          (princ "Speed commands\n==============\n")
          (mapc #'org-print-speed-command
                ;; FIXME: don't check `org-speed-commands-user' past 9.6
                my-org-item-key-bindings))
        (with-current-buffer "*Help*"
          (setq truncate-lines t)))
      (defvar my-org-item-key-bindings
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
          ("k" . (lambda () (forward-char) (org-mark-element) (call-interactively #'kill-region)))
          ("Clock Commands")
          ("I" . org-clock-in)
          ("O" . org-clock-out)
          ("Help")
          ("?" . my-org-item-speed-command-help)))
      (defun my-org-item-speed-command-activate (keys)
        (when (and (bolp)
                   (org-at-item-p))
          (cdr (assoc keys my-org-item-key-bindings))))
      
      (defun insert-zero-width-space()
        (interactive)
        (insert-char #x200b))
      (defun insert-zero-width-space-twice()
        (interactive)
        (insert-zero-width-space)
        (insert-zero-width-space))
      (setq org-directory
            (expand-file-name
             (if (file-exists-p "~/git/notes")
                 "~/git/notes"
               (progn
                 (when(not (file-exists-p "~/org"))
                   (mkdir "~/org"))
                 "~/org"))))
      (defvar org-prettify-symbols-alist
        '(("#+begin_src" . "🖥️")
          ("#+end_src". "🖥️")))
      :config
      
      ;; org-habitモジュールを有効化
      (add-to-list 'org-modules 'org-habit)
      (add-to-list 'org-modules 'org-id)

      (push 'my-org-item-speed-command-activate
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
                      ((eq window-system 'ns) "UDEV Gothic JPDOC")
                      ((eq window-system 'x) "UDEV Gothic JPDOC"))))
        (dolist (face '(org-table
                        org-formula
                        org-date))
          (set-face-attribute face nil :family fontset)))

      (add-to-list 'face-font-rescale-alist
                   '(".*IPAゴシック.*" . 0.85))

      (when (equal system-type 'darwin)
        (setq org-plantuml-jar-path
              "/usr/local/opt/plantuml/libexec/plantuml.jar"))
      
      (setq org-tag-alist
            '(("ignore" . ?i) ("@OFFICE" . ?o) ("@HOME" . ?h) ("SHOPPING" . ?s)
              ("MAIL" . ?m) ("PROJECT" . ?p) ("備忘録" . ?b)))
      (setq org-capture-templates
            `(("i" "インボックス" entry
               (file ,(concat org-directory "/inbox.org"))
               "* %? %i\n%U\n")
              ;; ("h" "定期的にやること" entry
              ;;  (file ,(concat org-directory "habit.org"))
              ;;  "* %?\n %U\n")
              ("t" "タスク" entry
               (file ,(concat org-directory "/task.org"))
               "* TODO %? %i\n%U\n")
              ("e" "イベント" entry
               (file ,(concat org-directory "/event.org"))
               "* EVENT %?\n %a\n%U\n")
              ("n"
               "ノート(本文から書く)"
               entry
               (file+headline, (concat org-directory "/notes.org") "MEMO")
               "* %U \n%?")
              ("N"
               "ノート(見出しから書く)"
               entry
               (file+headline, (concat org-directory "/notes.org") "MEMO")
               "* %U %?\n\n\n")
              ("r" "読みかけ(リンク付き)" entry
               (file ,(concat org-directory "/reading.org"))
               "* %?\n %a\n %U\n")
              ("m"
               "みんなで会議"
               entry
               (file+olp+datetree (concat org-directory "/minutes.org") "会議")
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
               (file+headline (concat org-directory "/gtd.org") "GTD")
               "** TODO %T %?\n   Entered on %U    %i\n"
               :empty-lines 1)
              ("i"
               "itemのテスト"
               item
               (file+headline (concat org-directory "/gtd.org") "GTD")
               "** TODO %T %?\n   Entered on %U    %i\n"
               :empty-lines 1)
              ("z"
               "'あれ'についてのメモ"
               entry
               (file+headline , (concat org-directory "/notes.org") "MEMO")
               "* %U %? %^g\n\n"
               :empty-lines 1)))
      ;;
      (setq org-agenda-default-appointment-duration 60)
      ;; コードを評価するとき尋ねない
      (setq org-confirm-babel-evaluate nil)

      (add-to-list 'org-babel-tangle-lang-exts
                   '("C" . "c"))

      (setq org-use-speed-commands t)
      (setq org-icalendar-alarm-time 30)
      (setq org-icalendar-timezone "Asia/Tokyo")

      ;; htmlで数式
      (setq org-html-mathjax-template
            "<script type=\"text/x-mathjax-config\">
    MathJax.Hub.Config({
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": { scale: %SCALE,
                        linebreaks: { automatic: \"%LINEBREAKS\" },
                        webFont: \"%FONT\"
                       },
        SVG: {scale: %SCALE,
              linebreaks: { automatic: \"%LINEBREAKS\" },
              font: \"%FONT\"},
        NativeMML: {scale: %SCALE},
        TeX: {
               extensions: [\"cancel.js\"],
               equationNumbers: {autoNumber: \"%AUTONUMBER\"},
               MultLineWidth: \"%MULTLINEWIDTH\",
               TagSide: \"%TAGSIDE\",
               TagIndent: \"%TAGINDENT\"
             }
});
</script>
<script src=\"%PATH\"></script>")
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
               :recursive t)))

      (leaf org-screenshot
        :url "https://dev.classmethod.jp/articles/org-mode-paste-show-clipboard-image/"
        :config
        (defun my-org-screenshot ()
          (interactive)
          (if (and (eq system-type 'darwin)
                   (equal (shell-command-to-string "command -v pngpaste") ""))
              (error "not found 'pngpaste' command"))
          (let ((filename (format "%s/img/%s_%s.png"
                                  org-directory
                                  (format-time-string "%Y%m%d_%H%M%S")
                                  (make-temp-name "")))
                (cmd (if (eq system-type 'darwin) "pngpaste" "import")))
            (call-process cmd nil nil nil filename)
            (insert (format "[[file:%s]]" (file-relative-name filename)))
            (org-display-inline-images)))
        )

      (leaf org-monokakido
        :url ("https://alhassy.github.io/org-special-block-extras/#Links"
              "https://gist.github.com/skoji/936a89f5e1e7c6f93d4a216175408659")
        )
      (org-link-set-parameters
       "mkdictionaries"
       :follow
       (lambda (label)
         (call-process
          "open" nil 0 nil
          (concat "mkdictionaries:///?text=" label)))
       :export
       (lambda (label description backend)
         (if (memq backend '(html latex))
             (format (pcase backend
                       ('html "<a href=\"%s\">%s</a>")
                       ('latex "\\href{%s}{%s}")
                       (_ "I don’t know how to export that!"))
                     (concat "mkdictionaries:///?text=" label)
                     (or description label))
           (or description label))))
      (leaf org-image
        :url "https://misohena.jp/blog/2020-05-26-limit-maximum-inline-image-size-in-org-mode.html"
        :config
        (defcustom org-limit-image-size '(0.99 . 0.5) "Maximum image size") ;; integer or float or (width-int-or-float . height-int-or-float)

        (defun org-limit-image-size--get-limit-size (width-p)
          (let ((limit-size (if (numberp org-limit-image-size)
                                org-limit-image-size
                              (if width-p (car org-limit-image-size)
                                (cdr org-limit-image-size)))))
            (if (floatp limit-size)
                (ceiling (* limit-size (if width-p (frame-text-width) (frame-text-height))))
              limit-size)))

        (defvar org-limit-image-size--in-org-display-inline-images nil)

        (defun org-limit-image-size--create-image
            (old-func file-or-data &optional type data-p &rest props)

          (if (and org-limit-image-size--in-org-display-inline-images
                   org-limit-image-size
                   (null type)
                   ;;(image-type-available-p 'imagemagick) ;;Emacs27 support scaling by default?
                   (null (plist-get props :width)))
              ;; limit to maximum size
              (apply
               old-func
               file-or-data
               (if (image-type-available-p 'imagemagick) 'imagemagick)
               data-p
               (plist-put
                (plist-put
                 (org-plist-delete props :width) ;;remove (:width nil)
                 :max-width (org-limit-image-size--get-limit-size t))
                :max-height (org-limit-image-size--get-limit-size nil)))

            ;; default
            (apply old-func file-or-data type data-p props)))

        (defun org-limit-image-size--org-display-inline-images (old-func &rest args)
          (let ((org-limit-image-size--in-org-display-inline-images t))
            (apply old-func args)))

        (defun org-limit-image-size-activate ()
          (interactive)
          (advice-add #'create-image :around #'org-limit-image-size--create-image)
          (advice-add #'org-display-inline-images :around #'org-limit-image-size--org-display-inline-images))

        (defun org-limit-image-size-deactivate ()
          (interactive)
          (advice-remove #'create-image #'org-limit-image-size--create-image)
          (advice-remove #'org-display-inline-images #'org-limit-image-size--org-display-inline-images)))
      (leaf org-agenda
        :after org
        :custom
        ((org-agenda-span . 'fortnight)))
      (leaf org-refile
        :after (org org-agenda)
        :custom
        ((org-refile-use-outline-path . t)
         (org-outline-path-complete-in-steps . nil))
        :config
        (setq org-refile-targets
              `((nil . (:maxlevel . 2))
                ((,(format "%s/next-actions.org" org-directory)) . (:level . 1))
                ((,(format "%s/references.org" org-directory)) . (:level . 1))
                ))
        (leaf org-refile-source-log
          :url "https://emacs.stackexchange.com/questions/36390/add-original-location-of-refiled-entries-to-logbook-after-org-refile"
          :config
          ;; add custom logging instead
          (add-hook 'org-after-refile-insert-hook #'clavis-org-refile-add-refiled-from-note)

          (advice-add 'org-refile
                      :before
                      #'clavis-org-save-source-id-and-header)

          (defvar clavis-org-refile-refiled-from-id nil)
          (defvar clavis-org-refile-refiled-from-header nil)

          (defun clavis-org-save-source-id-and-header (_)
            "Saves refile's source entry's id and header name to
`clavis-org-refile-refiled-from-id' and
`clavis-org-refile-refiled-from-header'. If refiling entry is
first level entry then it stores file path and buffer name
respectively."
            (interactive)
            (save-excursion
              (if (org-up-heading-safe)
                  (progn
                    (setq clavis-org-refile-refiled-from-id (org-id-get nil t))
                    (setq clavis-org-refile-refiled-from-header
                          (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))
                (setq clavis-org-refile-refiled-from-id (buffer-file-name))
                (setq clavis-org-refile-refiled-from-header (buffer-name)))))

          (defun clavis-org-refile-add-refiled-from-note ()
            "Adds a note to entry at point on where the entry was refiled
from using the org ID from `clavis-org-refile-refiled-from-id'
and `clavis-org-refile-refiled-from-header' variables."
            (interactive)
            (when (and clavis-org-refile-refiled-from-id
                       clavis-org-refile-refiled-from-header)
              (save-excursion
                (let* ((note-format "- Refiled on [%s] from [[id:%s][%s]]\n")
                       (time-format (substring (cdr org-time-stamp-formats) 1 -1))
                       (time-stamp (format-time-string time-format (current-time))))
                  (goto-char (org-log-beginning t))
                  (insert (format note-format
                                  time-stamp
                                  clavis-org-refile-refiled-from-id
                                  clavis-org-refile-refiled-from-header))))
              (setq clavis-org-refile-refiled-from-id nil)
              (setq clavis-org-refile-refiled-from-header nil)))
          )
        )
      
      (leaf ob-java
        :custom
        ((org-babel-java-compiler . "javac -encoding UTF-8")))
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
        (add-to-list 'org-latex-packages-alist '("" "minted" t))
        (add-to-list 'org-latex-packages-alist '("" "cancel" t))
        (add-to-list 'org-latex-packages-alist '("" "siunitx" t))
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
        (let ((template-dir (file-name-as-directory
                             (locate-user-emacs-file "lisp/org/ox-latex/templates")))
              (section-list '(("\\section{%s}" . "\\section*{%s}")
                              ("\\subsection{%s}" . "\\subsection*{%s}")
                              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                              ("\\paragraph{%s}" . "\\paragraph*{%s}")
                              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
          (mapcar
           (lambda (filename)
             (add-to-list 'org-latex-classes
                          (append `(,(file-name-base filename))
                                  `(,(org-file-contents (format "%s/%s" template-dir filename)))
                                  section-list)))
           (cddr (directory-files
                  (locate-user-emacs-file "lisp/org/ox-latex/templates")))))

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
      )
  (elpaca ox-hugo
    (leaf ox-hugo
      :disabled t
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
                     (function org-hugo-new-subtree-post-capture-template)))))
  (elpaca org-re-reveal
    (leaf org-re-reveal
      :disabled t
      :after org))
  (elpaca org-gcal
    (leaf org-gcal
      :disabled t
      :if (file-exists-p "~/Dropbox/org/googlecalendar/org-gcal-config.el")
      :after org
      :require t
      :custom
      ((org-gcal-down-days . 180)
       (org-gcal-up-days . 180))
      :config
      (load "~/Dropbox/org/googlecalendar/org-gcal-config.el")))
  (elpaca (anki-editor :host github :repo "orgtre/anki-editor"
                       :remotes ("origin"
                                 ("fork" :host github :repo "furusi/anki-editor" :branch "master")))
    (leaf anki-editor
      :doc "Minor mode for making Anki cards with Org"
      :req "emacs-25" "request-0.3.0" "dash-2.12.0"
      :tag "emacs>=25"
      :url "https://github.com/louietan/anki-editor"
      :emacs>= 25
      :after embark
      :hook
      (anki-editor-mode-hook . (lambda ()
                                 (let* ((keymap (copy-keymap embark-region-map)))
                                   (define-key keymap (kbd "c")
                                     'my-anki-editor-cloze-region)
                                   (setq-local embark-region-map keymap))
                                 ))
      :init
      (defun my-anki-editor-cloze-region (_text)
        (call-interactively
         (lambda (&optional arg hint)
           (interactive "NNumber: \nsHint (optional): ")
           (anki-editor-cloze-region arg hint))))
      )
    )
  (elpaca org-brain
    (leaf org-brain
      :after org
      :require t
      :bind
      ((:org-mode-map
        ("C-c b" . org-brain-prefix-map)))
      ))
  (leaf org-pdf*
    :config
    (elpaca org-pdftools
      (leaf org-pdftools
        :after org
        :custom
        `((org-pdftools-root-dir . ,(concat (getenv "HOME") "/GoogleDrive/Books")))
        :hook (org-mode-hook . org-pdftools-setup-link)
        ))
    (elpaca org-noter
      (leaf org-noter
        :after (org)))
    (elpaca org-noter-pdftools
      (leaf org-noter-pdftools
        :after (org-noter)
        :require t))
    (elpaca pdf-tools
      (leaf pdf-tools
        ;; https://github.com/politza/pdf-tools#installation
        :mode (("\\.pdf\\'" . pdf-view-mode))
        :hook (pdf-view-mode-hook . (lambda ()
                                      (display-line-numbers-mode 0)))
        :custom ((pdf-view-use-scaling . t))
        :config
        ;; (setenv "PKG_CONFIG_PATH"
        ;;         (string-trim (shell-command-to-string "echo \"$(brew --prefix poppler)/lib/pkgconfig:$(brew --prefix libffi)/lib/pkgconfig:$(brew --prefix zlib)/lib/pkgconfig:$(brew --prefix)/lib/pkgconfig:/opt/X11/lib/pkgconfig\"")))
        (pdf-tools-install)
        (display-line-numbers-mode -1)
        (setq pdf-annot-activate-created-annotations t)
        (setq pdf-view-resize-factor 1.1)))
    )
  (elpaca org-download
    (leaf org-download
      :after org
      :require t
      :hook ((org-mode-hook . org-download-enable))))
  (leaf org-roam*
    :config
    (elpaca (emacsql-sqlite :protocol https :inherit t :depth 1
                            :host github :repo "magit/emacsql"
                            :files (:defaults "emacsql-sqlite.el" "emacsql-sqlite-common.el" "sqlite")))
    (elpaca org-roam
      (leaf org-roam
        :req "emacs-26.1" "dash-2.13" "org-9.4" "emacsql-20230228" "magit-section-3.0.0"
        :emacs>= 26.1
        :commands (org-roam-node-find)
        :custom
        ((org-roam-title-to-slug-function . (lambda (text) text))
         (org-roam-v2-ack . t)
         (org-roam-completion-everywhere . t))
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
        (setq org-roam-directory (format "%s/roam" org-directory))
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
        (setq org-roam-dailies-capture-templates
              '(("d" "default" entry
                 "* %?"
                 :target
                 (file+head+olp "%<%Y-%m>.org" "#+TITLE: %<%Y-%m>\n\n\n" ("%<%Y-%m-%d>"))
                 )))
        (leaf org-roam-protocol
          :require t
          :after org
          )
        ))
    (elpaca org-roam-ui
      (leaf org-roam-ui
        :req "emacs-27.1" "org-roam-2.0.0" "simple-httpd-20191103.1446" "websocket-1.13"
        :emacs>= 27.1
        :after org
        :custom ((org-roam-ui-sync-theme . t)
                 (org-roam-ui-follow . t)
                 (org-roam-ui-update-on-save . t))))
    (elpaca consult-org-roam
      (leaf consult-org-roam
        :doc "Consult integration for org-roam"
        :req "emacs-27.1" "org-roam-2.2.0" "consult-0.16"
        :tag "emacs>=27.1"
        :url "https://github.com/jgru/consult-org-roam"
        :emacs>= 27.1
        :after org-roam consult))
    )
  (leaf ox*
    :custom
    (org-export-allow-bind-keywords . t)
    :config
    (defvar org-export-directory nil
      "org-exportの出力先を指定する変数。buffer-local変数として指定する。")
    (defun org-export-output-file-name--set-directory (orig-fn extension &optional subtreep pub-dir)
      (setq pub-dir (or pub-dir org-export-directory))
      (funcall orig-fn extension subtreep pub-dir))
    (advice-add 'org-export-output-file-name :around 'org-export-output-file-name--set-directory)
    (elpaca (ox-slimhtml :host github :repo "emacsattic/ox-slimhtml"))
    (elpaca (ox-tailwind :host github :repo "vascoferreira25/ox-tailwind"))
    )
  (elpaca ox-pandoc
    (leaf ox-pandoc
      :after org
      :require t
      :if (or (file-exists-p "/usr/bin/pandoc")
              (file-exists-p "/usr/local/bin/pandoc")
              (file-exists-p "/opt/local/bin/pandoc")
              (file-exists-p "/opt/homebrew/bin/pandoc"))))
  (elpaca ox-asciidoc)
  (elpaca ox-gfm
    (leaf ox-gfm
      :require t
      :after org))
  (elpaca ox-rst
    (leaf ox-rst
      :after (org)
      :custom
      ((org-rst-headline-underline-characters . '(45 126 94 58 39 32 95)))))
  (elpaca ob-mermaid
    (leaf ob-mermaid
      :doc "org-babel support for mermaid evaluation"
      :tag "lisp"
      :after org
      :url "https://github.com/arnm/ob-mermaid"
      :custom (ob-mermaid-cli-path . "~/.npm/bin/mmdc")))
  (elpaca org-journal
    (leaf org-journal
      :require t
      :after org
      ;; :commands org-journal-new-entry
      :custom
      `((org-journal-file-type . 'monthly)
        (org-journal-date-format . "%F (%a)")
        (org-journal-time-format . "<%Y-%m-%d %R> ")
        (org-journal-file-format . "%Y%m.org")
        (org-journal-file-header . "# -*- mode: org-journal; -*-
#+STARTUP: showall")
        )
      :preface
      (defvar my-org-journal-repeat-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-f")   #'org-journal-next-entry)
          (define-key map (kbd "f")   #'org-journal-next-entry)
          (define-key map (kbd "n")   #'org-journal-next-entry)
          (define-key map (kbd "C-b")   #'org-journal-previous-entry)
          (define-key map (kbd "b")   #'org-journal-previous-entry)
          (define-key map (kbd "p")   #'org-journal-previous-entry)
          map
          ))
      :config
      (put 'org-journal-next-entry 'repeat-map 'my-org-journal-repeat-map)
      (put 'org-journal-previous-entry 'repeat-map 'my-org-journal-repeat-map)
      (setq org-journal-dir (concat org-directory "/journal/"))))
  (elpaca org-modern
    (leaf org-modern
      :doc "Modern looks for Org"
      :req "emacs-27.1"
      :tag "emacs>=27.1"
      :url "https://github.com/minad/org-modern"
      :require t
      :emacs>= 27.1
      :after org
      :custom
      (org-modern-star . nil)
      :config
      (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
      (dolist (face '(org-modern-date-active org-modern-date-inactive))
        (set-face-attribute face nil
                            :family "UDEV Gothic JPDOC"))
      (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
      (global-org-modern-mode)
      ))

  (elpaca ob-browser)
  (elpaca ox-epub)
  (elpaca ob-php
    (leaf ob-php
      :doc "Execute PHP within org-mode source blocks."
      :req "org-8"
      :tag "php" "babel" "org"
      :url "https://repo.or.cz/ob-php.git"
      :after org))

  (elpaca org-contrib
    (leaf org-contrib
      :require t
      :after org
      :config
      ;; 有効にする言語 デフォルトでは elisp のみ
      (org-babel-do-load-languages
       'org-babel-load-languages '((C          . t)
                                   (dot        . t)
                                   (emacs-lisp . t)
                                   (gnuplot    . t)
                                   (java       . t)
                                   (lisp       . t)
                                   (mermaid    . t)
                                   (org        . t)
                                   (perl       . t)
                                   (php        . t)
                                   (plantuml   . t)
                                   (python     . t)
                                   (ruby       . t)
                                   (scheme     . t)))
      ;;ob-plantuml
      (add-to-list 'org-babel-default-header-args:plantuml
                   '(:cmdline . "-charset utf-8"))
      ))
  (elpaca (org-fc :host github :repo "l3kn/org-fc" :files(:defaults "awk" "demo.org"))
    (leaf org-fc
      :after org
      :require t))
  (elpaca org-latex-impatient
    (leaf org-latex-impatient
      :doc "Preview org-latex Fragments Instantly via MathJax"
      :req "emacs-26" "s-1.8.0" "posframe-0.8.0" "org-9.3" "dash-2.17.0"
      :tag "tools" "tex" "emacs>=26"
      :url "https://github.com/yangsheng6810/org-latex-instant-preview"
      :emacs>= 26
      :hook (org-mode-hook . org-latex-impatient-mode)
      :config
      (setq org-latex-impatient-tex2svg-bin
            (cond
             ((eq system-type 'darwin)
              "/opt/homebrew/lib/node_modules/mathjax-node-cli/bin/tex2svg")
             (t
              "/usr/bin/tex2svg")
             ))))
  )
(elpaca org-tag-beautify
  (leaf org-tag-beautify
    :doc "Beautify Org mode tags"
    :req "emacs-26.1" "org-pretty-tags-0.2.2" "all-the-icons-5.0.0"
    :url "https://repo.or.cz/org-tag-beautify.git"
    :emacs>= 26.1
    :require t
    :custom
    `(org-tag-beautify-data-dir . ,(format "%sorg-tag-beautify/data/" elpaca-repos-directory))
    ))
(leaf anki-editor-org-src
  :after org
  ;; :leaf-defer nil
  :hook
  (org-src-mode-hook . (lambda ()
                         (if (string-match (regexp-quote "[ anki-editor ]") (buffer-name))
                             (anki-editor-mode))))
  :init
  (add-to-list 'org-src-lang-modes '("anki-editor" . org)))

(elpaca mermaid-mode)
(leaf mu4e
  :disabled t
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
           ("/archive" . ?a)))
  (leaf org-mu4e
    :disabled t
    :after (org mu4e)
    :config
    ;;store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil))
  )

(elpaca (yatex :repo "https://www.yatex.org/gitbucket/git/yuuji/yatex.git")
  (leaf yatex
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
              (lambda ()
                (auto-fill-mode -1)))
    (add-hook 'yatex-mode-hook
              (lambda ()
                (reftex-mode 1)
                (define-key reftex-mode-map
                  (concat YaTeX-prefix ">") 'YaTeX-comment-region)
                (define-key reftex-mode-map
                  (concat YaTeX-prefix "<") 'YaTeX-uncomment-region))))
  )
;; for yatex
(when (equal system-type 'darwin)
  (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:/Applications/Skim.app/Contents/SharedSupport:$PATH" t)
  (setq exec-path (append '("/usr/local/bin" "/Library/TeX/texbin" "/Applications/Skim.app/Contents/SharedSupport") exec-path)))
(elpaca php-mode
  (leaf php-mode
    :custom
    ((php-manual-url . 'ja)))
  )
(elpaca ac-php)
(elpaca flycheck-phpstan
  (leaf flycheck-phpstan
    :hook (php-mode-hook . (lambda ()
                             (require 'flycheck-phpstan)
                             (flycheck-mode t))))
  )
(elpaca company-php
  (leaf company-php
    :after (ac-php)
    :hook (php-mode-hook . (lambda ()
                             ;; Enable company-mode
                             (company-mode t)
                             ;; (require 'company-php)

                             ;; Enable ElDoc support (optional)
                             (ac-php-core-eldoc-setup)

                             (set (make-local-variable 'company-backends)
                                  '((company-ac-php-backend company-dabbrev-code)
                                    company-capf company-files)))))
  )
(elpaca typescript-mode
  (leaf typescript-mode
    :hook (typescript-mode-hook . #'lsp)))
(elpaca rainbow-mode)
(elpaca poetry)
(leaf pipenv
  :disabled t
  :hook (python-mode-hook . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))
(elpaca hydra)
(elpaca go-mode
  (leaf go-mode
    :after lsp-mode
    :hook (go-mode-hook . lsp-deferred))
  )
(when (version< emacs-version "29")
  (elpaca csharp-mode))
(elpaca android-mode)
(when (eq system-type 'darwin)
  (elpaca swift-mode
    (leaf swift-mode))
  (elpaca (lsp-sourcekit :host github :repo "emacs-lsp/lsp-sourcekit" :files (:defaults "lsp-sourcekit.el"))
    (leaf lsp-sourcekit
      :require t
      :after lsp
      :custom
      `(lsp-sourcekit-executable . ,(string-trim (shell-command-to-string "xcrun --find sourcekit-lsp")))))
  )
(elpaca ccls
  (leaf ccls
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
  )
(elpaca smartparens
  (leaf smartparens
    :diminish t
    :require smartparens-config
    :hook (elpaca-after-init-hook . smartparens-global-mode)
    :bind
    (:emacs-lisp-mode-map
     ("C-c C-u" . sp-backward-up-sexp)
     ("C-c C-n" . sp-next-sexp)
     ("C-c C-p" . sp-previous-sexp)))
  )
(elpaca kotlin-mode
  (leaf kotlin-mode
    :mode (("\\.kt\\'" . kotlin-mode)))
  )
(elpaca (dart-mode :host github :repo "bradyt/dart-mode")
  (leaf dart-mode
    :doc "Major mode for editing Dart files"
    :req "emacs-24.3"
    :tag "languages" "emacs>=24.3"
    :url "https://github.com/bradyt/dart-mode"
    :require t
    :mode (("\\.dart\\'" . dart-mode))
    :hook ((dart-mode-hook . lsp-deferred)
           (dart-mode-hook . (lambda ()
                               (electric-pair-mode 1)
                               (when (featurep 'embark)
                                 (setq-local embark-target-finders
                                             (append (remove
                                                      'embark-target-file-at-point
                                                      embark-target-finders)
                                                     '(embark-target-file-at-point)))
                                 ))))))
(elpaca fsharp-mode)
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
(elpaca plantuml-mode
  (leaf plantuml-mode
    :custom
    ((plantuml-default-exec-mode . 'jar)
     (plantuml-output-type . "png"))
    :config
    (setq plantuml-jar-path
          (cond ((eq system-type 'darwin)
                 "/usr/local/opt/plantuml/libexec/plantuml.jar")
                ((string-match "ndeavour" my-lsb-distribution-name)
                 "/usr/share/java/plantuml/plantuml.jar")
                (t ""))))
  )
(elpaca htmlize)
(elpaca adoc-mode
  (leaf adoc-mode
    :bind
    (:adoc-mode-map
     ("C-c C-n" . outline-next-visible-heading)
     ("C-c C-p" . outline-previous-visible-heading)))
  )
(elpaca pandoc)
(elpaca graphviz-dot-mode)
(elpaca editorconfig
  (leaf editorconfig
    :require t
    :diminish editorconfig-mode
    :config
    (editorconfig-mode 1))
  )
(leaf easy-hugo
  :disabled t
  :custom
  ((easy-hugo-org-header . t)
   (easy-hugo-default-ext . ".org")))
(elpaca npm-mode)
(elpaca autodisass-java-bytecode)
(elpaca solarized-theme)
(elpaca modus-themes
  (leaf modus-themes
    :require t
    :custom
    ((modus-themes-italic-constructs . t)
     (modus-themes-org-blocks . 'gray-background)
     (modus-themes-custom-auto-reload . t)
     (modus-themes-disable-other-themes . t)
     (modus-vivendi-palette-overrides  . '((bg-main "gray20")))
     (modus-operandi-palette-overrides  . '((bg-main "#F6F6EF")))
     )
    :config
    (defcustom my-current-modus-theme 'modus-operandi
      "my selected theme"
      :type '(choice (const :tag "modus-vivendi(黒)" modus-vivendi)
                     (const :tag "modus-operandi(白)" modus-operandi))
      :initialize #'custom-initialize-default
      :group 'my-group
      )
    (load-theme 'modus-operandi-tinted :no-confim)
    )
  )
(elpaca markdown-mode
  (leaf markdown-mode
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :custom ((markdown-command . "multimarkdown"))
    :config
    (when (eq window-system 'ns)
      (set-face-attribute 'markdown-table-face nil
                          :family "IPAGothic"))))

(elpaca docker)

(elpaca docker-compose-mode)

(elpaca review-mode
  (leaf review-mode
    :mode (("\\.re\\'" . review-mode))))

(elpaca csv-mode)

(elpaca gnuplot)

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
(elpaca ssh-config-mode)

(elpaca fish-mode)

(elpaca dockerfile-mode
  (leaf dockerfile-mode
    :require t
    :config
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
  )

(elpaca meson-mode)

(elpaca git-modes
  (leaf git-modes
    :require t))
(elpaca groovy-mode)
(leaf server
  :commands (server-running-p)
  :hook
  (emacs-startup-hook . (lambda ()
                          (unless (server-running-p)
                            (server-start))))
  )

;; https://gist.github.com/tek-nishi/a7fc3933be5e62c7eeaa
(defun my-insert-newline-and-indent(arg)
  "カーソル行の上や下に一行挿入してインデント(ARGが４だと上の行に挿入)."
  (interactive "p")
  (let ((p (if (eq arg 4)
               1
             2)))
    (move-beginning-of-line p)
    (open-line 1)
    (indent-for-tab-command)))

;;from https://uwabami.github.io/cc-env/Emacs.html
(defun my-make-scratch (&optional arg)
  "SCRATCHバッファを作成する.
Optional argument ARG hoge."
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
(defun my-buffer-name-list ()
  "バッファのリストを作成する."
  (mapcar (function buffer-name) (buffer-list)))
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら
          ;; *scratch* バッファを新しく作る.
          (function
           (lambda ()
             (unless (member "*scratch*" (my-buffer-name-list))
               (my-make-scratch 1)))))

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
(elpaca vimrc-mode
  (leaf vimrc-mode
    :require t
    :mode
    ("\\.vim\\(rc\\)?\\'" . vimrc-mode)))
(elpaca (lsp-bridge :host github :repo "manateelazycat/lsp-bridge"
                    :files (:defaults "lsp_bridge.py" "acm/*" "core" "langserver" "multiserver" "resources"))
  (leaf lsp-bridge
    :require t
    :bind
    (:lsp-bridge-mode-map
     ("C-c C-l a a" . lsp-bridge-code-action))))
(leaf *lsp
  :config
  (elpaca (lsp-mode ;; :ref "dfda673"
           )
    (leaf lsp-mode
      :require 'lsp
      :commands (lsp lsp-deferred)
      :custom ((lsp-auto-execute-action . nil)
               (lsp-completion-provider . :none) ;disable company-capf
               (lsp-keymap-prefix . "C-c C-l")
               (lsp-semantic-tokens-enable . t)
               )
      :hook ((lsp-mode-hook  . lsp-enable-which-key-integration)
             (lsp-completion-mode-hook . my-lsp-mode-setup-completion)
             (c-mode-hook    . lsp-deferred)
             (css-mode-hook  . lsp-deferred)
             (html-mode-hook . lsp-deferred))
      :init
      (setq read-process-output-max (* 1024 1024))
      (defun my-lsp-mode-setup-completion ()
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
              '(orderless)))))
  (elpaca lsp-python-ms
    (leaf lsp-python-ms
      :disabled t
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
      ))
  (elpaca lsp-pyright
    (leaf lsp-pyright
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
      ))
  ;; optionally
  (elpaca lsp-ui
    (leaf lsp-ui
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
      ))
  (leaf lsp-treemacs
    :commands lsp-treemacs-errors-list
    :config
    (lsp-treemacs-sync-mode 1))
  ;; optionally if you want to use debugger
  (elpaca lsp-java
    (leaf lsp-java
      :disabled t
      :require t
      :hook (java-mode-hook . (lambda ()
                                (lsp-deferred)
                                (setq lsp-managed-mode t)))
      :bind ((:lsp-mode-map
              ("M-." . lsp-find-definition)))))
  (elpaca lsp-metals
    (leaf lsp-metals
      :custom
      ((lsp-metals-server-args . '("-J-Dmetals.allow-multiline-string-formatting=off")))
      :hook (scala-mode-hook . lsp-deferred)
      ))
  (elpaca lsp-dart)
  (elpaca lsp-tailwindcss)
  (elpaca dap-mode
    (leaf dap-mode
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
        :after (lsp-java))))
  (elpaca consult-lsp
    (leaf consult-lsp
      :after (consult lsp-mode)
      :config
      (consult-customize
       consult-lsp-symbols
       :preview-key (kbd "C-,"))
      ))
  )
(elpaca flymake)
(leaf eglot
  :after corfu flymake
  :bind
  ((:eglot-mode-map
    ("C-c C-l a a" . eglot-code-actions)))
  :config
  ;; (add-hook 'rustic-mode-hook 'eglot-ensure)
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  )
(elpaca eglot-java
  (leaf eglot-java
    :doc "Java extension for the eglot LSP client"
    :req "emacs-26.1" "eglot-1.0" "jsonrpc-1.0.0"
    :tag "languages" "convenience" "emacs>=26.1"
    :url "https://github.com/yveszoundi/eglot-java"
    :emacs>= 26.1
    :require t
    :custom
    ((eglot-java-prefix-key . "C-c C-l"))
    :config
    ;; (eglot-java-init)
    )
  )
(elpaca consult-eglot
  (leaf consult-eglot
    :doc "A consulting-read interface for eglot"
    :req "emacs-27.1" "eglot-1.7" "consult-0.9"
    :tag "lsp" "completion" "tools" "emacs>=27.1"
    :url "https://github.com/mohkale/consult-eglot"
    :emacs>= 27.1
    :after eglot consult)
  )
(elpaca vterm
  (leaf vterm
    :require t))
(elpaca (elfeed :files (:defaults "web/*"))
  (leaf elfeed
    :doc "an Emacs Atom/RSS feed reader"
    :req "emacs-24.3"
    :tag "emacs>=24.3"
    :url "https://github.com/skeeto/elfeed"
    :emacs>= 24.3
    :bind ((:elfeed-show-mode-map
            ("S-SPC" . scroll-down-command))
           (:elfeed-search-mode-map
            ("j" . forward-line)
            ("n" . forward-line)
            ("k" . (lambda () (interactive)(forward-line -1)))
            ("p" . (lambda () (interactive)(forward-line -1)))
            ("e" . (lambda () (interactive)(eww (my-elfeed-yank-entry-url))))
            ;;osascript -e 'tell application "Safari" to add reading list item "http://totl.net/"'
            ("a" .(lambda ()
                    (interactive)
                    (if (and (eq system-type 'darwin)
                             (equal (shell-command-to-string "command -v osascript") ""))
                        (error "not found 'osascript' command"))
                    (let ((url (car (mapcar #'elfeed-entry-link (elfeed-search-selected)))))
                      (call-process-shell-command
                       (format "osascript -e 'tell application \"Safari\" to add reading list item \"%s\"'" (my-elfeed-yank-entry-url)))
                      (message "The selected entry added to Safari's reading list.")
                      (elfeed-search-untag-all-unread))))
            ))
    :custom
    ((elfeed-search-date-format . '("%Y-%m-%d %H:%M" 16 :left))
     (elfeed-search-filter . "@6-months-ago +unread +"))
    :config
    (defun my-elfeed-yank-entry-url ()
      (interactive)
      (let ((url (car (mapcar #'elfeed-entry-link (elfeed-search-selected)))))
        (if (equal url "")
            (error "Selected entry's url is empty")
          url
          )))
    (require 'elfeed-web)
    )
  )
(elpaca elfeed-goodies
  (leaf elfeed-goodies
    :doc "Elfeed goodies"
    :req "popwin-1.0.0" "powerline-2.2" "elfeed-2.0.0" "cl-lib-0.5" "link-hint-0.1"
    :url "https://github.com/algernon/elfeed-goodies"
    :require elfeed-goodies popwin
    :after elfeed
    :bind (:elfeed-search-mode-map
           :package (elfeed elfeed-goodies)
           ("l" . elfeed-goodies/toggle-logs))
    :custom
    ((elfeed-goodies/entry-pane-position . 'bottom))
    :config
    (elfeed-goodies/setup)
    )
  )

(elpaca powershell)

;; Major mode for Twitter http://twmode.sf.net/
(elpaca twittering-mode)
(elpaca lua-mode)
(elpaca protobuf-mode)
(elpaca pcre2el
  (leaf pcre2el
    :doc "regexp syntax converter"
    :req "emacs-24" "cl-lib-0.3"
    :tag "emacs>=24"
    :url "https://github.com/joddie/pcre2el"
    :custom
    ((reb-re-syntax . 'pcre))
    :bind
    (:reb-mode-map
     ("C-r" . reb-prev-match)
     ("C-s" . reb-next-match)))
  )
(elpaca regex-tool
  (leaf regex-tool
    :doc "A regular expression evaluation tool for programmers"
    :tag "development" "programming" "languages" "regex"
    :url "http://www.newartisans.com/"
    :custom
    (regex-tool-backend . 'perl)
    :bind
    (:regex-tool-mode-map
     ("C-c C-q" . regex-tool-quit)))
  )
(elpaca visual-regexp-steroids
  (leaf visual-regexp-steroids
    :doc "Extends visual-regexp to support other regexp engines"
    :req "visual-regexp-1.1"
    :tag "feedback" "visual" "python" "replace" "regexp" "foreign" "external"
    :url "https://github.com/benma/visual-regexp-steroids.el/"
    :require t
    :config
    (setq vr/command-python (replace-regexp-in-string "^python "  "python3 " vr--command-python-default))
    )
  )

(leaf repeat-mode
  :emacs>= 28
  :custom
  (repeat-exit-key . "q")
  :config
  (repeat-mode t))

;; Instant GitHub-flavored Markdown/Org preview using grip.
(elpaca grip-mode)
(leaf grip-mode
  :bind ((:markdown-mode-command-map
          ("g" . grip-mode))))
(elpaca tree-sitter-langs
  (leaf tree-sitter
    :disabled t
    :doc "Incremental parsing system"
    :req "emacs-25.1" "tsc-0.18.0"
    :tag "tree-sitter" "parsers" "tools" "languages" "emacs>=25.1"
    :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
    :emacs>= 25.1
    :require tree-sitter-langs
    :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
    :config
    (global-tree-sitter-mode)))
(elpaca dirvish)
(elpaca shrface
  (leaf shrface
    :doc "Extend shr/eww with org features and analysis capability"
    :req "emacs-25.1" "org-9.0" "language-detection-0.1.0"
    :tag "faces" "emacs>=25.1"
    :url "https://github.com/chenyanming/shrface"
    :emacs>= 25.1
    :require t
    :hook (eww-after-render-hook . shrface-mode)
    :config
    (shrface-basic)
    (shrface-trial)
    (shrface-default-keybindings) ; setup default keybindings
    (setq shrface-href-versatile t)

    (leaf shrface-nov
      :after nov
      :init (add-hook 'nov-mode-hook #'shrface-mode)
      :config
      (setq nov-shr-rendering-functions '((img . nov-render-img)
                                          (title . nov-render-title)))
      (setq nov-shr-rendering-functions (append nov-shr-rendering-functions
                                                shr-external-rendering-functions))
      (define-key nov-mode-map (kbd "n") 'org-next-visible-heading)
      (define-key nov-mode-map (kbd "p") 'org-previous-visible-heading)
      (define-key nov-mode-map (kbd "s") 'org-toggle-narrow-to-subtree)
      (define-key nov-mode-map (kbd "u") 'outline-up-heading))
    )
  )
(elpaca nov
  (leaf nov
    :doc "Featureful EPUB reader mode"
    :req "esxml-0.3.6" "emacs-25.1"
    :tag "epub" "multimedia" "hypermedia" "emacs>=25.1"
    :url "https://depp.brause.cc/nov.el"
    :emacs>= 25.1
    :mode ("\\.epub\\'" . nov-mode))
  )
(elpaca (nov-xwidget :host github :repo "chenyanming/nov-xwidget")
  (leaf nov-xwidget
    :after nov
    :require t
    :config
    (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
    (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))
  )

(elpaca speed-type
  (leaf speed-type
    :doc "Practice touch and speed typing"
    :req "emacs-25.1"
    :tag "games" "emacs>=25.1"
    :url "https://github.com/parkouss/speed-type"
    :emacs>= 25.1
    :require t
    :config
    (with-eval-after-load 'speed-type
      (add-hook 'speed-type-mode-hook
                (lambda ()
                  (when (and (featurep 'corfu)
                             global-corfu-mode)
                    (corfu-mode -1)))))
    ))

(add-to-list 'load-path (expand-file-name (locate-user-emacs-file "lisp")))
(require 'my-misc)
(require 'my-window)

(let ((f "~/Dropbox/.config/emacs/config.el"))
  (when (file-exists-p f)
    (load-file f)))

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'magit-diff-edit-hunk-commit 'disabled nil)
