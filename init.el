;;; init.el --- my init script  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;; (require 'profiler)
;; (profiler-start 'cpu)

;;elpaca
(defvar elpaca-installer-version 0.11)
(let* ((package-directory (expand-file-name "elpaca/" (format "%spackages/" user-emacs-directory )))
       (package-directory-version (expand-file-name emacs-version package-directory)))
  (defvar elpaca-cache-directory (expand-file-name "cache/" package-directory))
  (defvar elpaca-directory package-directory-version)
  (defvar elpaca-builds-directory (expand-file-name "builds/" package-directory-version))
  (defvar elpaca-repos-directory (expand-file-name "repos/" package-directory-version)))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(setq elpaca-queue-limit (if (eq system-type 'windows-nt)
                             15
                           30))
(when (eq system-type 'windows-nt)
  (elpaca-no-symlink-mode 1))
;;elpaca end
(if (version< emacs-version "29")
    (elpaca use-package (require 'use-package)))

;; <leaf-install-code>
(eval-and-compile
  (use-package package
    :config
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize))

  (use-package leaf :ensure t)

  (leaf leaf-keywords
    :ensure t
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))

  (leaf leaf-convert
    :doc "Convert many format to leaf format"
    :req "emacs-26.1" "leaf-3.6.0" "leaf-keywords-1.1.0" "ppp-2.1"
    :tag "tools" "emacs>=26.1"
    :url "https://github.com/conao3/leaf-convert.el"
    :emacs>= 26.1
    :ensure t
    :after leaf leaf-keywords ppp))

(leaf elpaca
  :custom
  ((elpaca-log-diff-function . #'elpaca-log-magit-diff))
  :bind
  ((:elpaca-ui-mode-map
    :package elpaca-ui
    ("j" . next-line)
    ("k" . previous-line)
    ("p" . previous-line)
    ("P" . elpaca-ui-mark-pull))
   (:elpaca-log-mode-map
    :package elpaca-log
    ("D" . elpaca-ui-mark-delete)
    ("d" . elpaca-log-view-diff))))

(leaf no-littering
  :elpaca t
  :require t)

(leaf diminish
  :ensure t
  :diminish (show-paren-mode))
;; </leaf-install-code>


(show-paren-mode t)
(column-number-mode)
(require 'cl-lib)

(defcustom my-share-dir (expand-file-name "~/Sync")
  "Dropbox directory."
  :type 'directory
  :set (lambda (symbol value) (set symbol (expand-file-name value))))
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
  (when (file-exists-p custom-file)
    (load-file custom-file)))

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
    ;; (garbage-collection-messages . t) ; GC発動のタイミングを確認するときに有効にする
    (eol-mnemonic-dos . "(CRLF)")
    (eol-mnemonic-mac . "(CR)")
    (eol-mnemonic-unix . "(LF)")
    (use-short-answers . t)
    (indent-tabs-mode . nil)
    (inhibit-startup-screen . t)
    (mark-ring-max . 128)
    (package-user-dir . ,(locate-user-emacs-file (format "elpa/%s" emacs-version)))
    (set-mark-command-repeat-pop . t)    ;; C-u C-SPCの後C-SPCだけでマークを遡れる
    (show-paren-style . 'mixed)
    (tramp-ssh-controlmaster-options . "-4") ; ssh接続時にipv4アドレスを利用する
    (use-dialog-box . nil)
    (use-file-dialog . nil)
    (vc-follow-symlinks . t)
    (vc-handled-backends . '(Git))))
(leaf recentf
  :after no-littering
  :custom `((recentf-auto-cleanup . 'never)
            (recentf-max-menu-items . 30)
            (recentf-max-saved-items . 2000))
  :global-minor-mode recentf-mode)
(leaf ediff
  :custom ((ediff-diff-options . "-w")
           (ediff-split-window-function . 'split-window-horizontally)
           (ediff-window-setup-function . 'ediff-setup-windows-plain)))
(leaf pcmpl-git
  :defer-config
  ;; https://misohena.jp/blog/2024-03-03-fix-browser-open-use-git-in-shell-mode.html
  (when (eq system-type 'windows-nt)
    (defun my-pcomplete/git:around (oldfun)
    (cl-letf* ((old-pcomplete-from-help (symbol-function 'pcomplete-from-help))
               ((symbol-function 'pcomplete-from-help)
                (lambda (command &rest args)
                  (apply old-pcomplete-from-help
                         (if (and
                              (listp command)
                              (equal (take 2 command) `(,vc-git-program "help"))
                              (cddr command)
                              (not (string-prefix-p "-" (caddr command))))
                             `(,(car command) ,(caddr command) "-h")
                           command)
                         args))))
      (funcall oldfun)))
    (advice-add 'pcomplete/git :around #'my-pcomplete/git:around)))
(leaf magit
  :elpaca transient
  :elpaca (magit :files ("lisp/magit*.el"
          "lisp/git-rebase.el" "lisp/git-commit.el" "docs/magit.texi"
          "docs/AUTHORS.md" "LICENSE"
          "Documentation/magit.texi"
          "Documentation/AUTHORS.md" (:exclude
                                      "lisp/magit-libgit.el"
                                      "lisp/magit-libgit-pkg.el"
                                      "lisp/magit-section.el"
                                      "lisp/magit-section-pkg.el")))
  :bind (("C-x g" . magit-status)
         (:magit-diff-mode-map
          ("=" . magit-diff-more-context))
         (:project-prefix-map :package project
                              ("v" . magit-status)))
  :hook
  (ediff-keymap-setup-hook . add-d-to-ediff-mode-map)
  :custom
  ((magit-display-buffer-function . 'magit-display-buffer-fullframe-status-v1)
   (magit-diff-refine-hunk . 'all)
   (magit-refresh-status-buffer . nil))
  :init
  (defun my-magit-mode-bury-buffer ()
    (interactive)
    (call-interactively #'magit-mode-bury-buffer)
    (when (< 1(length (tab-bar-tabs)))
      (tab-close)))
  ;; https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    (when (ediff-merge-job)
      (keymap-set ediff-mode-map "d" 'ediff-copy-both-to-C)
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
  ;; ediff時にorgファイルを全て表示する
  (defun my-ediff-prepare-buffer-function ()
    (org-fold-show-all))
  (with-eval-after-load 'org-element-ast
    (add-hook 'ediff-prepare-buffer-hook #'my-ediff-prepare-buffer-function)))
(leaf *vertico
  :config
  (leaf vertico
    :emacs>= 27.1
    :elpaca (vertico :host github :repo "minad/vertico"
                     :files (:defaults "extensions/*.el"))
    :bind ((:vertico-map
            ("M-RET" . minibuffer-force-complete-and-exit)
            ("M-TAB" . minibuffer-complete)
            ("C-r" . vertico-previous)
            ("C-s" . vertico-next)))
    :custom
    ((vertico-count . 20)
     (vertico-cycle . t)
     (vertico-resize . t)
     ;; Do not allow the cursor in the minibuffer prompt
     (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt))
     ;; Enable recursive minibuffers
     (enable-recursive-minibuffers . t))
    :global-minor-mode t
    :config
    (when (< emacs-major-version 31)
      (advice-add #'completing-read-multiple :filter-args
                  (lambda (args)
                    (cons (format "[CRM%s] %s"
                                  (string-replace "[ \t]*" "" crm-separator)
                                  (car args))
                          (cdr args)))))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))
  (leaf vertico-multiform
    :disabled t
    :after consult vertico
    :custom
    ((vertico-multiform-categories . '((consult-grep
                                        buffer
                                        (vertico-buffer-display-action . (display-buffer-same-window))))))
    :config
    (vertico-multiform-mode)
    (setq vertico-multiform-commands
          `((consult-imenu buffer ,(lambda (_) (text-scale-set -1)))
            (consult-outline buffer ,(lambda (_) (text-scale-set -1)))))
    ;; Disable preview for consult-grep commands
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       :preview-key nil))

  (leaf vertico-repeat
    :bind (("C-x c r" . vertico-repeat-previous)
           ("C-x c R" . vertico-repeat-select))
    :hook
    (minibuffer-setup-hook . vertico-repeat-save))
  (leaf vertico-directory
    :bind ((:vertico-map
            ("RET"    . vertico-directory-enter)
            ("DEL"    . vertico-directory-delete-char)
            ("M-DEL"  . vertico-directory-delete-word)
            ("C-l"    . vertico-directory-up)))
    ;; Tidy shadowed file names
    :hook
    (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))
  (leaf vertico-quick
    :custom
    ((vertico-quick1 . "aoeu")
     (vertico-quick2 . "htns")))
  ;; Use the `orderless' completion style.
  ;; Enable `partial-completion' for files to allow path expansion.
  ;; You may prefer to use `initials' instead of `partial-completion'.
  (leaf orderless
    :elpaca t
    :custom
    ((completion-category-defaults  . nil)
     (completion-category-overrides . '((file (styles basic partial-completion))))
     (completion-styles             . '(orderless basic))))
  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (leaf consult
    :elpaca (consult :host github :repo "minad/consult") consult-projectile
    :custom
    ((consult-async-min-input . 2)
     (consult-narrow-key . ">")
     (consult-project-function . #'projectile-project-root)
     (consult-ripgrep-args
      . "rg --hidden --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")
     (xref-show-definitions-function . #'consult-xref)
     (xref-show-xrefs-function . #'consult-xref))
    :bind (("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-x C-SPC" . consult-global-mark)
           ("C-x b" . consult-buffer)
           ("C-x c i" . consult-imenu)
           ("M-g i" . consult-imenu)
           ("M-g m" . consult-mark)
           ("C-x j" . consult-recent-file)
           ("C-x r j" . consult-register)
           ("C-x r l"  . consult-bookmark)
           ("M-y" . consult-yank-pop)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x 5 b" . consult-buffer-other-frame)
           ("C-x r SPC" . consult-register-store)
           ("C-h i" . consult-info)
           ([remap goto-line] . consult-goto-line)
           (:isearch-mode-map :package isearch
                              ("C-i" . my-consult-line)
                              ("M-e" . consult-isearch-history)))
    :hook
    (completion-list-mode-hook . consult-preview-at-point-mode)
    :init
    (defun my-consult-line (&optional at-point)
      (interactive "P")
      (if at-point
          (consult-line (thing-at-point 'symbol))
        (consult-line)))
    :config
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
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args))))
  (leaf affe
    :elpaca t
    :after consult
    :custom
    ((affe-highlight-function . 'orderless-highlight-matches)
     (affe-regexp-function . 'orderless-compile))
    :config
    ;; Manual preview key for `affe-grep'
    (consult-customize affe-grep :preview-key "M-."))
  (leaf marginalia
    :elpaca t
    :bind (("M-A" . marginalia-cycle)
           (:minibuffer-local-map
            ("M-A" . marginalia-cycle)))
    :init
    (marginalia-mode))
  (leaf embark
    :elpaca (embark :files (:defaults ("embark-org.el" "embark-consult.el")))
    :emacs>= 26.1
    :hook
    (embark-collect-mode-hook . consult-preview-at-point-mode)
    :bind
    `((,(if window-system "C-." "M-.") . embark-act)         ;; pick some comfortable binding
      ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
      (:embark-package-map
       ("b" . embark-browse-package-url))
      (:embark-region-map
       ("C-l" . my-lookup-mkdict)
       ("j" . join-line))
      (:embark-symbol-map
       ("C-l" . my-lookup-mkdict))
      (:embark-identifier-map
       ("C-l" . my-lookup-mkdict)))
    :init
    ;; Optionally replace the key help with a completing-read interface
    ;; (setq prefix-help-command #'embark-prefix-help-command)
    (defun my-lookup-mkdict (str)
      (interactive "sInput: ")
      (call-process
       "open" nil 0 nil
       (concat "mkdictionaries:///?text=" str)))
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))
    (defun my-advice--fixup-whitespace (old-fn &rest args)
      "Skip function execution when cursor is between Japanese characters"
      (if (and (looking-at-p " *\\cj")
               (looking-back "\\cj *" (point-beginning-of-line)))
          (cycle-spacing 0)
        (apply old-fn args)))
    (advice-add 'fixup-whitespace :around #'my-advice--fixup-whitespace))
  (leaf all-the-icons-completion
    :elpaca t
    :doc "Add icons to completion candidates"
    :req "emacs-26.1" "all-the-icons-5.0"
    :tag "lisp" "convenient" "emacs>=26.1"
    :url "https://github.com/iyefrat/all-the-icons-completion"
    :emacs>= 26.1
    :after all-the-icons
    :config
    (all-the-icons-completion-mode t))
  (defcustom my-completion-frontend 'corfu
    "Completion backend to use. Can be 'corfu, 'completion-preview, or nil."
    :type '(choice (const :tag "Use Corfu" corfu)
                   (const :tag "Use Completion Preview" completion-preview)
                   (const :tag "None" nil))
    :group 'my-custom-group)
  (leaf completion-preview
    :if (eq my-completion-frontend 'completion-preview)
    :bind
    (:completion-preview-active-mode-map
     ("M-n" . completion-preview-next-candidate)
     ("M-p" . completion-preview-prev-candidate))
    :custom
    (completion-preview-minimum-symbol-length . 2)
    :global-minor-mode global-completion-preview-mode)
  (leaf corfu
    :if (eq my-completion-frontend 'corfu)
    :elpaca (corfu :host github :repo "minad/corfu" :depth 10 :files (:defaults "extensions/*.el"))
    :url "https://github.com/minad/corfu"
    :emacs>= 27.1
    :bind
    (:corfu-map
     ("M-SPC" . corfu-insert-separator)
     ("M-m" . corfu-move-to-minibuffer))
    :custom
    ((completion-cycle-threshold . 3)
     (corfu-preselect . 'prompt)
     (corfu-auto . t)
     (corfu-cycle . t)
     (corfu-exclude-modes . '(rustic-mode rust-mode))
     (tab-always-indent . 'complete)
     (corfu-on-exact-match . nil))
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
    (global-corfu-mode)
    (corfu-popupinfo-mode)
    (corfu-history-mode t)
    (savehist-mode t)
    (add-to-list 'savehist-additional-variables 'corfu-history)
    (with-eval-after-load 'dabbrev
      (dolist (mode '(tags-table-mode skk-jisyo-mode))
        (push mode dabbrev-ignored-buffer-modes))))
  (leaf corfu-terminal
    :if (and (< emacs-major-version 31)
             (null (display-graphic-p)))
    :elpaca t
    :after corfu popon
    :config
    (unless (display-graphic-p)
      (corfu-terminal-mode +1)))
  (leaf popon
    :elpaca t
    :init
    (unless (display-graphic-p)
      (require 'popon)))
  (leaf tempel
    :elpaca t
    :doc "Tempo templates/snippets with in-buffer field editing"
    :req "emacs-27.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/tempel"
    :emacs>= 27.1
    :bind
    ((("M-+" . tempel-complete) ;; Alternative tempel-expand
      ("M-*" . tempel-insert))
     (:tempel-map
      ("C-i" . tempel-next)))
    :custom
    `((tempel-path . ,(format "%ssnippets/tempel/templates/*" user-emacs-directory)))
    :init
    (defun tempel-setup-capf ()
      ;; Add the Tempel Capf to `completion-at-point-functions'. `tempel-expand'
      ;; only triggers on exact matches. Alternatively use `tempel-complete' if
      ;; you want to see all matches, but then Tempel will probably trigger too
      ;; often when you don't expect it.
      ;; NOTE: We add `tempel-expand' *before* the main programming mode Capf,
      ;; such that it will be tried first.
      (add-hook 'completion-at-point-functions
                #'tempel-expand nil 'local))
    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf))
  (elpaca tempel-collection)
  (leaf cape
    :elpaca t
    :doc "Completion At Point Extensions"
    :req "emacs-27.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/cape"
    :emacs>= 27.1
    :custom
    (cape-dict-limit . 20000)
    :bind-keymap
    ("C-c f" . cape-prefix-map)
    :init
    (if (memq system-type '(darwin gnu/linux))
        (customize-set-variable 'cape-dict-file "/usr/share/dict/words"))
    (add-hook 'completion-at-point-functions #'cape-keyword)
    (add-hook 'completion-at-point-functions #'cape-tex)
    (add-hook 'completion-at-point-functions #'cape-file)

    (when-let* ((worddir (expand-file-name ".config/emacs/cape" my-share-dir))
                (wordfiles (and (file-directory-p worddir)
                                (cddr (directory-files worddir t)))))
      (setopt cape-dict-file (if (consp cape-dict-file)
                                 (append wordfiles cape-dict-file)
                               (cons cape-dict-file wordfiles))))

    (defun my-cape-wrap-with-annotation (oldfn &optional annotstr)
      (when (null annotstr)
        (setq annotstr "from unknown function"))
      (cape-wrap-properties oldfn
                            :annotation-function
                            (lambda (_) (format " %s" annotstr))))
    :config
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (add-hook 'completion-at-point-functions
                          (cape-capf-inside-code #'cape-elisp-symbol) nil 'local)))
    (advice-add 'pcomplete-completions-at-point
                :around
                (lambda (oldfn &rest _)
                  (my-cape-wrap-with-annotation
                   oldfn
                   (symbol-name 'pcomplete-completions-at-point))))
    (defun my/org-get-radio-targets ()
      (let ((targets '()))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "<<\\([^<>]+\\)>>" nil t)
            (push (match-string-no-properties 1) targets)))
        (delete-dups (nreverse targets))))

    (defun my/cape-org-radio-targets ()
      (when (derived-mode-p 'org-mode)
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (when bounds
            (list (car bounds)
                (cdr bounds)
                (my/org-get-radio-targets)
                :exclusive 'no)))))

    (defun my/cape-dabbrev+radio ()
      "dabbrev + org radio target の複合補完を即時実行。"
      (interactive)
      (let ((completion-at-point-functions
             (list (cape-capf-super
                    #'my/cape-org-radio-targets
                    #'cape-dabbrev))))
        (completion-at-point)))

    (define-key cape-prefix-map (kbd "d") #'my/cape-dabbrev+radio))
  (leaf kind-icon
    :elpaca (kind-icon :host github :repo "jdtsmith/kind-icon")
    :emacs>= 27.1
    :after corfu
    :custom
    ((kind-icon-blend-background . t)
     (kind-icon-default-face . 'corfu-default))
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(leaf conf-mode
  :config
  (push '("\\.toml\\'" . conf-toml-mode) auto-mode-alist))

(leaf whitespace-mode
  :custom
  (whitespace-style . '(face tabs trailing space-before-tab newline
                             indentation empty space-after-tab tab-mark)))
(leaf emacs29
  :emacs>= 29
  :config
  (when window-system
    (pixel-scroll-precision-mode)))

(leaf custom-darwin
  :if (eq system-type 'darwin)
  :custom
  ((browse-url-firefox-program . "/Applications/Firefox.app/Contents/MacOS/firefox")
   (browse-url-firefox-new-window-is-tab . t)))

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
    (setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")))


(leaf xref
  :defvar auto-read-only-dirs
  :hook (xref-after-jump-hook .
                              (lambda ()
                                (dolist (f auto-read-only-dirs)
                                  (when (string-match-p (expand-file-name f) buffer-file-name)
                                    (view-mode)))))
  :config
  (defvar auto-read-only-dirs
    `("/opt/homebrew/Cellar/"
      ,lisp-directory
      "~/.cargo/registry/"
      ,(expand-file-name "packages/" user-emacs-directory)
      "~/.rustup/toolchains/")))
(leaf view-mode
  :bind
  (:view-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ;; ("SPC". scroll-up-command)
   ;; ("S-SPC". scroll-down-command)
   ))
(leaf info
  :bind
  (:Info-mode-map
   ("j" . next-line)
   ("k" . previous-line))
  :config
  (defun Info-find-node--info-ja (orig-fn filename &rest args)
    (apply orig-fn
           (pcase filename
             ("emacs" "emacs-ja")
             ("elisp" "elisp-ja")
             (_ filename))
           args))
  (let ((infopath (getenv "INFOPATH")))
    (if (and (stringp infopath)
             (string-match-p ".local/share/emacs" infopath))
        (advice-add 'Info-find-node :around 'Info-find-node--info-ja))))

(leaf deepl-translate
  :url "https://uwabami.github.io/cc-env/Emacs.html"
  :commands my-deepl-translate
  :bind
  (:embark-region-map
   :package embark
   ("T" . my-deepl-translate))
  :preface
  (require 'url-util)
  (defun my-translate--sanitize-string (string)
    "docstring"
    (replace-regexp-in-string
     "|" (regexp-quote "\x005c\x007c")
     (replace-regexp-in-string
      "/"
      (regexp-quote "\x005c\x002f")
      string)))
  (defvar my-translate-url  "https://miraitranslate.com/trial/#en/ja/"
    ;; "https://www.deepl.com/translator#en/ja/%s"
    )
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
    (let* ((string (my-translate--sanitize-string string))
           (url (format "%s%s"
                        my-translate-url (url-hexify-string string))))
      (cond ((eq system-type 'darwin)
             (browse-url-default-macosx-browser url))
            ((string-match ".*-microsoft-standard-WSL2.*" operating-system-release)
             (browse-url-generic url))
            (t
             (browse-url-firefox url))))))
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
(leaf helpful
  :disabled t
  :doc "A better *help* buffer"
  :req "emacs-25" "dash-2.18.0" "s-1.11.0" "f-0.20.0" "elisp-refs-1.2"
  :tag "lisp" "help" "emacs>=25"
  :url "https://github.com/Wilfred/helpful"
  :elpaca t
  :emacs>= 25
  :bind
  ((:help-map
    :package help
    ("v" . helpful-variable)
    ("f" . helpful-callable)
    ("o" . helpful-symbol)
    ("k" . helpful-key))
   (:embark-symbol-map
    :package embark
    ("h" . helpful-symbol))))
(leaf diff-mode
  :bind
  (:diff-mode-map
   ("v" . scroll-up-command)
   ("V" . scroll-down-command))
  :hook
  (diff-mode-hook . (lambda () (read-only-mode t))))
(leaf autorevert
  :hook
  (emacs-startup-hook . global-auto-revert-mode))
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
  :config
  (defvar-keymap my-scroll-other-window-repeat-map
    :repeat t
    "v"   #'scroll-other-window
    "C-v" #'scroll-other-window
    "M-v" #'scroll-other-window-down
    "V" #'scroll-other-window-down))
(leaf simple
  :config
  (defcustom my-read-only-dirs nil
    "List of directories where files should be opened in read-only.
Each element in the list is a string, representing a directory path.
When a file is opened and its path starts with one of the directory paths in this list,
read-only-mode will be activated for that file."
    :type '(repeat string))
  (defun my-read-only-find-file-hook ()
    (when (cl-some
           (lambda (dir)
             (string-prefix-p (expand-file-name dir) buffer-file-name))
           my-read-only-dirs)
      (read-only-mode 1)))
  (add-hook 'find-file-hook 'my-read-only-find-file-hook))
(leaf initchart
  :elpaca (initchart :host github :repo "yuttie/initchart")
  :disabled t
  :require t
  :config
  (initchart-record-execution-time-of load file)
  (initchart-record-execution-time-of require feature))
(leaf esup
  :elpaca t
  :require t)
(defun which-linux-distribution ()
  "Return string which obtains from 'lsb_release' command."
  (interactive)
  (if (eq system-type 'gnu/linux)
      (string-trim (shell-command-to-string "lsb_release -sd")
                   "^\"" "\"?[ \t\n\r]+")
    ""))
(setq my-lsb-distribution-name
      (which-linux-distribution))

;;行番号を表示
(if (version< "26" emacs-version)
    (progn
      ;; (global-display-line-numbers-mode)
      (setq-default indicate-empty-lines t)
      (setq-default indicate-buffer-boundaries 'left)))
(leaf exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :elpaca t
  :config
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path-from-shell-variables "PYTHONPATH")
  (add-to-list 'exec-path-from-shell-variables "JAVA_HOME"))
(leaf mise
  :if (executable-find "mise")
  :elpaca t
  :global-minor-mode global-mise-mode)
(leaf system-packages
  :elpaca t
  :config
  (cond
   ((eq system-type 'darwin)
    (setq system-packages-package-manager 'brew))
   ((string-match-p "asahi" operating-system-release)
    (setq system-packages-package-manager 'dnf))
   ((string-match-p "manjaro\\|endeavouros" operating-system-release)
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
          system-packages-package-manager 'yay))))
(leaf bind-key
  :bind
  (("M-<f1>" . other-frame)  ;Macのショートカットに合わせる
   ;; ("C-o" . my-insert-newline-and-indent)
   (:isearch-mode-map
    ("C-o" . isearch-exit))
   (:reb-mode-map
    :package re-builder
    ("C-c C-k". reb-quit))))
(leaf outline-repeat
  :after outline
  :config
  (defvar-keymap my-outline-navigation-repeat-map
    :parent outline-navigation-repeat-map
    :repeat t
    "TAB" #'outline-cycle))
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
      (,(kbd "0") . ,(ins-val ")")) (,(kbd ")") . ,(ins-val "0")) (,[kp-0] . ,(ins-val "0")))))
(when (equal system-type 'darwin)
  (setq ns-command-modifier 'meta)
  (when (memq window-system '(ns mac))
    ;; 游教科書体
    ;; (set-face-attribute 'default nil
    ;;                     :family "YuKyokasho Yoko")
    ;; UDEV Gothic
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
      ;; (custom-theme-set-faces
      ;;  'user
      ;;  `(org-level-8 ((t (,@headline ,@variable-tuple))))
      ;;  `(org-level-7 ((t (,@headline ,@variable-tuple))))
      ;;  `(org-level-6 ((t (,@headline ,@variable-tuple))))
      ;;  `(org-level-5 ((t (,@headline ,@variable-tuple))))
      ;;  `(org-level-4 ((t (,@headline ,@variable-tuple))))
      ;;  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
      ;;  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
      ;;  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
      ;;  `(org-table ((t (,@headline ,@variable-tuple))))
      ;;  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil)))))
      )))
(when (equal system-type 'gnu/linux)
  (add-to-list 'load-path "~/opt/mu-1.0/mu4e")
  ;;曖昧な文字幅を指定する
  (aset char-width-table ?→ 2)
  (when (memq window-system '(x pgtk))
    (set-face-attribute 'default nil :family "UDEV Gothic JPDOC")))
;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
(setq use-default-font-for-symbols nil)
(when (version< emacs-version "29")
  (elpaca restart-emacs))

(leaf dired
  :custom
  ((dired-dwim-target . (lambda () (unless current-prefix-arg (dired-dwim-target-next))))
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
(leaf pomodoro
  :doc "A timer for the Pomodoro Technique"
  :elpaca t
  :commands pomodoro-start
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
  (when (not (member '(pomodoro-mode-line-string pomodoro-mode-line-string)  mode-line-format))
    (pomodoro-add-to-mode-line)))
(elpaca sudo-edit)
(leaf japanese-holidays
  :after calendar
  :elpaca t
  :doc "Calendar functions for the Japanese calendar"
  :req "emacs-24.1" "cl-lib-0.3"
  :tag "calendar" "emacs>=24.1"
  :url "https://github.com/emacs-jp/japanese-holidays"
  :emacs>= 24.1
  :require t
  :custom
  (
   (calendar-mark-holidays-flag . t)    ;祝日をカレンダーに表示
   ;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
   (japanese-holiday-weekend . '(0 6))  ;土日を祝日として表示
   (japanese-holiday-weekend-marker . '(holiday nil nil nil nil nil japanese-holiday-saturday))) ;土曜日を水色で表示
  :config
  ;; 他の国の祝日も表示させたい場合は適当に調整
  (setq calendar-holidays (append japanese-holidays holiday-local-holidays
                                  holiday-other-holidays))
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend))
(elpaca (libgit2 :repo "https://github.com/magit/libegit2.git"
                 :main "libgit.el"))
(elpaca magit-svn)
(leaf blamer
  :doc "Show git blame info about current line"
  :req "emacs-27.1" "posframe-1.1.7"
  :tag "emacs>=27.1"
  :url "https://github.com/artawower/blamer.el"
  :emacs>= 27.1
  :elpaca t)
(leaf tab-bar
  :bind
  (:tab-prefix-map
   ("k" . tab-bar-close-tab-by-name)))
(leaf projectile
  :elpaca t
  :bind `(,(when (version< "28" emacs-version)
             '(:projectile-command-map
               ("v" . my-projectile-vc-in-new-tab))))
  :bind-keymap (("C-c p" . projectile-command-map))
  :custom
  `((projectile-sort-order . 'recently-active)
    (projectile-switch-project-action . 'projectile-commander))
  :init
  (let ((dir (locate-user-emacs-file (format "projectile/%s" emacs-version))))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (defun my-projectile-vc-in-new-tab ()
    (interactive)
    (let ((tab-name-list (mapcar #'cdadr (tab-bar-tabs)))
          (tab-name (format "=p:%s"
                            (replace-regexp-in-string
                             elpaca-repos-directory "/REPO/"
                             (projectile-acquire-root))))
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
        (projectile-vc)))))
  :config
  (projectile-mode +1)
  (dolist
      (d '("^\\.ccls-cache$"))
    (push d projectile-globally-ignored-directories))
  (when (string> emacs-version "28")
    (def-projectile-commander-method ?v "Open project root in vc-dir or magit."
                                     (my-projectile-vc-in-new-tab))))
;; ddskk
(leaf ddskk
  :elpaca (ddskk :host github :repo "furusi/ddskk" :depth 10
                 :files ("context-skk.el" "ddskk*.el" "skk*.el" "tar-util.el"
                         "doc/skk.texi" "etc/skk.xpm" "ccc.el"
                         (:exclude "skk-xemacs.el" "skk-lookup.el"))
                 :remotes ("fork"
                           ("origin" :host github :repo "skk-dev/ddskk" :branch "master")))
  :commands skk-mode
  :bind (("C-x C-j" . skk-mode)
         (:minibuffer-local-map
          ("C-j" . skk-kakutei)))
  :hook ((skk-load-hook . (lambda () (require 'context-skk))) ;自動的に英字モードになる
         (skk-jisyo-edit-mode-hook . (lambda () (read-only-mode t))))
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
    (skk-kutouten-type . 'jp)
    (skk-save-jisyo-instantly . t)
    (skk-search-katakana . 'jisx0201-kana)
    (skk-search-sagyo-henkaku . t)   ;サ行変格活用の動詞も送りあり変換出来るようにする
    (skk-share-private-jisyo . t)
    (skk-sticky-key . '(?, ?.))
    (skk-use-act . t)                ;全角・半角カタカナを変換候補にする
    (skk-use-jisx0201-input-method . t)
    (skk-user-directory . ,(locate-user-emacs-file "ddskk"))
    (skk-japanese-message-and-error . t))
  :init
  (leaf skk-dropbox
    :if (file-exists-p (expand-file-name ".config/ddskk" my-share-dir))
    :custom
    (skk-jisyo-code . 'utf-8))

  (let ((skk-jisyo-directory
         (if (file-exists-p (expand-file-name ".config/ddskk/skkdic-utf8" my-share-dir))
             (expand-file-name ".config/ddskk/skkdic-utf8" my-share-dir)
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
    (keymap-global-set "C-x C-j" 'skk-mode))
  (leaf skk-study
    :require t)
  (leaf skk-hint
    :require t
    :custom
    ;; ▼モード中で=漢字の読み方を指定する
    (skk-hint-start-char . ?=))
  (leaf context-skk
    :config
    (dolist (mode '(python-mode js-mode rustic-mode dart-mode
                                go-mode typescript-mode))
      (add-to-list 'context-skk-programming-mode mode))
    (setq context-skk-mode-off-message "[context-skk] 日本語入力 off")
    (defun my-context-skk-at-heading-p ()
      (and (bolp)
           (and (memq major-mode '(org-mode org-journal-mode))
                (or (org-at-heading-p)
                    (org-at-item-p)
                    (org-at-block-p)
                    (org-at-item-checkbox-p)))))
    (add-hook 'org-mode-hook
              (lambda ()
                (setq-local
                 context-skk-context-check-hook
                 '(my-context-skk-at-heading-p
                   context-skk-in-read-only-p))))
    (context-skk-mode 1))
  (defun skk-set-display-table ()
    (walk-windows (lambda (w)
                    (let ((disptab (or buffer-display-table
                                       (make-display-table))))
                      (aset disptab ?\▼ (vector (make-glyph-code ?# 'escape-glyph)))
                      (aset disptab ?\▽ (vector (make-glyph-code ?@ 'escape-glyph)))
                      (set-window-display-table w disptab)))))
  (require 'ccc)
  (add-hook 'window-configuration-change-hook #'skk-set-display-table)
  (add-hook 'after-init-hook #'skk-set-display-table))
(leaf eww
  :commands (eww)
  :bind
  ((:eww-mode-map
    ("j" . next-line)
    ("k" . previous-line))
   (:embark-url-map
    :package embark
    ("x" . browse-url-default-browser)))
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

(leaf migemo
  :elpaca t
  :if (and (not (eq system-type 'windows-nt))
       (executable-find "cmigemo"))
  :require t
  :custom
  `((migemo-options . '("--quiet" "--nonewline" "--emacs"))
    (migemo-coding-system . 'utf-8-unix)
    (migemo-user-dictionary . nil)
    (migemo-regex-dictionary . nil)
    (migemo-dictionary .
                       ,(cond ((eq system-type 'darwin)
                               "/opt/homebrew/opt/cmigemo/share/migemo/utf-8/migemo-dict")
                              ((eq system-type 'windows-nt)
                               "~/.local/share/migemo/cp932/migemo-dict")
                              ((string-match-p "arch" operating-system-release)
                               "/usr/share/migemo/utf-8/migemo-dict")
                              (t "/usr/share/cmigemo/utf-8/migemo-dict"))))
  :config
  ;; https://www.yewton.net/2022/02/07/consult-ripgrep-migemo/
  (with-eval-after-load 'consult
    (defun consult--migemo-regexp-compiler (input type ignore-case)
      (setq input (mapcar #'migemo-get-pattern (consult--split-escaped input)))
      (cons (mapcar (lambda (x) (consult--convert-regexp x type)) input)
            (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
              (apply-partially #'consult--highlight-regexps regexps ignore-case))))
    (setq consult--regexp-compiler #'consult--migemo-regexp-compiler))
  (migemo-init))
;; SLIMEのロード
(leaf undo-tree
  :elpaca t
  :diminish (global-undo-tree-mode undo-tree-mode)
  :global-minor-mode global-undo-tree-mode
  :custom
  ((undo-tree-history-directory-alist . '(("." . "~/.emacs.d/undo-tree")))
   (undo-tree-incompatible-major-modes . '(term-mode fundamental-mode))
   (undo-tree-visualizer-diff . t)))
(leaf vundo :elpaca t)
(elpaca undo-fu)
(leaf undo-fu-session
  :elpaca t
  :doc "Persistent undo, available between sessions"
  :req "emacs-28.1"
  :tag "convenience" "emacs>=28.1"
  :url "https://codeberg.org/ideasman42/emacs-undo-fu-session"
  :emacs>= 28.1
  :custom
  (undo-fu-session-incompatible-files . '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :global-minor-mode undo-fu-session-global-mode)
(leaf display-line-numbers
  :custom
  (display-line-numbers-type . 'relative)
  :hook
  (prog-mode-hook . display-line-numbers-mode))
(leaf eglot
  :bind
  ((:eglot-mode-map
    ("C-c C-l a a" . eglot-code-actions)
    ("C-c C-l r r" . eglot-rename)
    ("C-c C-l w r" . eglot-reconnect)
    ("C-c C-l w q" . eglot-shutdown))))
(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :elpaca (eglot-booster :type git
                         :host github
                         :repo "jdtsmith/eglot-booster")
  :after eglot
  :global-minor-mode eglot-booster-mode)
(leaf treesit
  :custom
  ((treesit-language-source-alist .
                                  '((go "https://github.com/tree-sitter/tree-sitter-go")
                                    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))))
(leaf *lsp
  :config
  (leaf lsp-mode
    :elpaca (lsp-mode :depth 1)
    :commands (lsp lsp-deferred)
    :custom ((lsp-auto-execute-action . nil)
             (lsp-completion-provider . :none) ;disable company-capf
             (lsp-keymap-prefix . "C-c C-l")
             (lsp-semantic-tokens-enable . t)
             (lsp-inlay-hint-enable . t))
    :hook ((lsp-mode-hook  . lsp-enable-which-key-integration)
           (lsp-completion-mode-hook . my-lsp-mode-setup-completion)
           (c-mode-hook    . lsp-deferred)
           (css-mode-hook  . lsp-deferred)
           (html-mode-hook . lsp-deferred))
    :init
    (setq read-process-output-max (* 1024 1024))
    (defun my-lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
    :config
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                (setcar orig-result command-from-exec-path))
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    (when (executable-find "emacs-lsp-booster")
      (advice-add (if (progn (require 'json)
                             (fboundp 'json-parse-buffer))
                      'json-parse-buffer
                    'json-read)
                  :around
                  #'lsp-booster--advice-json-parse)
      (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))
  (leaf lsp-python-ms
    :elpaca t
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
    )
  (leaf lsp-pyright
    :elpaca t
    :hook
    ((python-mode-hook . (lambda ()
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
                   "[/\\\\]__pycache__$"))
      (push dir lsp-file-watch-ignored)))
  ;; optionally
  (leaf lsp-ui
    :elpaca t
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
    :elpaca t
    :disabled t
    :require t
    :hook (java-mode-hook . (lambda ()
                              (lsp-deferred)
                              (setq lsp-managed-mode t)))
    :bind ((:lsp-mode-map
            ("M-." . lsp-find-definition))))
  ;; (elpaca lsp-metals
  ;;   (leaf lsp-metals
  ;;     :custom
  ;;     ((lsp-metals-server-args . '("-J-Dmetals.allow-multiline-string-formatting=off")))
  ;;     :hook (scala-mode-hook . lsp-deferred)))
  (elpaca lsp-dart)
  (elpaca lsp-tailwindcss)
  (leaf dap-mode
    :elpaca t
    :after lsp-mode
    :global-minor-mode dap-mode dap-ui-mode
    :require dap-cpptools dap-gdb-lldb
    :config
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
    :elpaca t
    :after (consult lsp-mode)
    :config
    (consult-customize
     consult-lsp-symbols
     :preview-key (kbd "C-,"))))
(leaf rust-mode
  :elpaca t
  :doc "A major-mode for editing Rust source code"
  :req "emacs-25.1"
  :tag "languages" "emacs>=25.1"
  :url "https://github.com/rust-lang/rust-mode"
  :emacs>= 25.1
  :custom ((rust-mode-treesitter-derive . nil)))
(leaf rustic
  :elpaca t
  :doc "Rust development environment"
  :req "emacs-26.1" "rust-mode-1.0.3" "dash-2.13.0" "f-0.18.2"
  "let-alist-1.0.4" "markdown-mode-2.3" "project-0.3.0" "s-1.10.0"
  "seq-2.3" "spinner-1.7.3" "xterm-color-1.6"
  :tag "languages" "emacs>=26.1"
  :emacs>= 26.1
  :after rust-mode
  :custom ((rustic-ansi-faces . ["black" "red3" "green3" "yellow3"
                                 "deep sky blue" "magenta3" "cyan3" "white"])
           (rustic-lsp-client . 'lsp-mode))
  :hook
  ((rustic-mode-hook . my-rustic-init))
  :init
  (with-eval-after-load 'smartparens
    (push 'rustic-mode sp-ignore-modes-list))
  (defun my-rustic-init ()
    (electric-pair-local-mode t)
    (when (featurep 'embark)
      (setq-local embark-target-finders
                  (append (remove
                           'embark-target-file-at-point
                           embark-target-finders)
                          '(embark-target-file-at-point))
                  corfu-auto-prefix 2)))
  (leaf rustic-babel
    :after org
    :require t))
(leaf rustowl
  :if (executable-find "rustowl")
  :elpaca (rustowlsp :host github :repo "cordx56/rustowl" :main "rustowl.el"))
(leaf lsp-haskell
  :elpaca t
  :doc "Haskell support for lsp-mode"
  :req "emacs-24.3" "lsp-mode-3.0"
  :tag "haskell" "emacs>=24.3"
  :url "https://github.com/emacs-lsp/lsp-haskell"
  :emacs>= 24.3
  :hook (haskell-mode-hook . lsp-deferred))
(leaf ron-mode
  :elpaca t
  :doc "Rusty Object Notation mode"
  :req "emacs-24.5.1"
  :tag "languages" "emacs>=24.5.1"
  :url "https://chiselapp.com/user/Hutzdog/repository/ron-mode/home"
  :emacs>= 24.5
  :mode (("\\.ron$" . ron-mode)))
(leaf moody
  :elpaca t
  :doc "Tabs and ribbons for the mode line"
  :req "emacs-25.3" "compat-28.1.1.0"
  :url "https://github.com/tarsius/moody"
  :custom
  ((x-underline-at-descent-line . t))
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))
(leaf dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))
(leaf wgrep
  :elpaca t
  :custom
  ((wgrep-enable-key . "e")
   (wgrep-auto-save-buffer . t)))
(elpaca highlight-symbol)
(elpaca expand-region)
(leaf expand-region
  :bind (("C-=" . er/expand-region)))
(when (display-graphic-p)
  (elpaca all-the-icons))
(leaf which-key
  :elpaca `(,@(version< emacs-version "30"))
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
(leaf yasnippet*
  :elpaca yasnippet yasnippet-snippets consult-yasnippet
  :config
  (leaf yasnippet
    :diminish yas-minor-mode
    :config
    (yas-global-mode 1)
    (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets/yasnippet")))
  (leaf consult-yasnippet
    :doc "A consulting-read interface for yasnippet"
    :req "emacs-27.1" "yasnippet-0.14" "consult-0.9"
    :tag "emacs>=27.1"
    :url "https://github.com/mohkale/consult-yasnippet"
    :emacs>= 27.1
    :after yasnippet consult))
(elpaca gitignore-templates)

(leaf rst
  :bind ((:rst-mode-map
          ("M-RET" . rst-insert-list)))
  :config
  (when (eq system-type 'darwin)
    (setq rst-pdf-program "open -a Skim")
    (setq rst-slides-program "open -a Firefox")))
(leaf gradle-mode
  :elpaca t
  :mode (("\\.gradle$" . gradle-mode)))
(leaf slime
  :elpaca t
  :if (executable-find "ros")
  :custom
  ((inferior-lisp-program . "ros -Q run")
   (slime-auto-start . 'ask)
   (slime-net-coding-system . 'utf-8-unix))
  :hook ((lisp-mode-hook . slime-mode))
  :config
  ;; (slime-setup '(slime-fancy slime-company))
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
  ;; (setq common-lisp-hyperspec-symbol-table
  ;;       (format "%sData/Map_Sym.txt" common-lisp-hyperspec-root)
  ;;       common-lisp-hyperspec-issuex-table
  ;;       (format "%sData/Map_IssX.txt" common-lisp-hyperspec-root))
  )
(leaf slime-company
  :elpaca t
  :after slime
  :custom ((slime-company-completion . 'fuzzy)
           (slime-complete-symbol*-fancy . t))
  :hook ((slime-repl-mode-hook
          . (lambda () (add-to-list
                        'company-backends
                        '(company-slime company-dabbrev-code))))))
(leaf web-mode
  :elpaca t
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
(defmacro my-org-push-src-lang-modes (mode  &optional name)
   (cond
    ((null name) (setq name (symbol-name mode)))
    ((symbolp name) (setq name (symbol-name name))))
   `(with-eval-after-load 'org
      (push '(,name . ,mode) org-src-lang-modes)))
(leaf org*
  :config
  (leaf org
    :elpaca `,@(and (not (eq system-type 'windows-nt))
                    '(org :depth 1))
    :mode (("\\.org$" . org-mode))
    :hook ((org-mode-hook . (lambda () (prettify-symbols-mode)))
           (org-mode-hook . (lambda () (setq prettify-symbols-alist org-prettify-symbols-alist)))
           (org-mode-hook . (lambda ()
                              ;; org-modeの固定幅フォントを設定
                              (dolist (face '(org-table
                                              org-formula
                                              org-date))
                                (set-face-attribute face nil :family "UDEV Gothic JPDOC")))))
    :custom
    `((org-directory . ,(expand-file-name
                         (cond
                          ((file-exists-p (expand-file-name "org" my-share-dir)) (expand-file-name "org" my-share-dir))
                          ((file-exists-p "~/git/notes") "~/git/notes")
                          (t (progn
                               (when (not (file-exists-p "~/org"))
                                 (mkdir "~/org"))
                               "~/org")))))
      (org-link-frame-setup .
                            '((vm . vm-visit-folder-other-frame)
                              (vm-imap . vm-visit-imap-folder-other-frame)
                              (gnus . org-gnus-no-new-news)
                              (file . find-file)
                              (wl . wl-other-frame)))
      (org-link-file-path-type . (lambda (path)
                                   (let* ((truepath (file-truename path))
                                          (proj (project-current))
                                          (root (if proj (project-current) default-directory)))
                                     (if (string-prefix-p (expand-file-name (pcase (project-current)
                                                                              (`(projectile . ,dir) dir)
                                                                              (_ root)))
                                                          truepath)
                                         (file-relative-name truepath)
                                       (abbreviate-file-name path)))))
      (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w)" "SOMEDAY(s)" "|" "DONE(d)")
                             (sequence "|" "CANCELED")))
      (org-agenda-default-appointment-duration . 60)
      (org-agenda-start-on-weekday . 0)
      (org-clock-persist . t)
      (org-confirm-babel-evaluate . nil)
      (org-enforce-todo-checkbox-dependencies . t)
      (org-enforce-todo-dependencies . t)
      (org-export-allow-bind-keywords . t)
      (org-export-backends . '(ascii html icalendar latex md odt asciidoc pandoc gfm))
      (org-export-with-sub-superscripts . '{})
      (org-export-with-toc . nil)
      (org-icalendar-alarm-time . 30)
      (org-icalendar-timezone . "Asia/Tokyo")
      (org-icalendar-use-scheduled . '(event-if-todo todo-start))
      (org-id-link-consider-parent-id . t)
      (org-id-link-to-org-use-id . 'use-existing)
      (org-list-allow-alphabetical . t)
      (org-log-into-drawer . t)
      (org-preview-latex-default-process . 'dvisvgm)
      (org-return-follows-link . t)
      (org-special-ctrl-a/e . t)
      (org-src-preserve-indentation . t)
      (org-startup-folded . t)
      (org-use-sub-superscripts . '{})
      )
    :bind (("C-c c" . org-capture)
           ("C-c l" . org-store-link)
           ("C-c a" . org-agenda)
           ("<f2>" . insert-zero-width-space)
           (:org-mode-map
            ("C-c C-\'" . org-insert-structure-template)))
    :init
    (defun my-org-item-speed-command-help ()
      "Show the available speed commands for item."
      (interactive)
      (let ((org-speed-commands my-org-item-key-bindings))
        (call-interactively #'org-speed-command-help)))
    (defvar my-org-item-key-bindings
      '(("p" . org-previous-item)
        ("n" . org-next-item)
        ("U" . org-metaup)
        ("D" . org-metadown)
        ("r" . org-metaright)
        ("l" . org-metaleft)
        ("R" . org-shiftmetaright)
        ("L" . org-shiftmetaleft)
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
    (defvar org-prettify-symbols-alist
      nil
      ;; '(("#+begin_src" . "🖥️")
      ;;   ("#+end_src". "🖥️"))
      )
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

    (add-to-list 'face-font-rescale-alist
                 '(".*IPAゴシック.*" . 0.85))

    (when-let* ((path (executable-find "plantuml")))
      (setq org-plantuml-jar-path path))

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
    (add-to-list 'org-babel-tangle-lang-exts
                 '("C" . "c"))
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

    (defun my-org-choose-src-language ()
      (let ((lang (completing-read
                   "Choose language: "
                   (mapcar (lambda (x)
                             (car x))
                           org-src-lang-modes)
                   nil nil)))
        (format "src %s" lang)))

    (defun my-org-insert-structure-template (orig-fun &rest args)
      (if (equal (car args) "src")
          (apply orig-fun (list (my-org-choose-src-language)))
        (apply orig-fun args)))

    (advice-add 'org-insert-structure-template :around #'my-org-insert-structure-template)

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
          (org-display-inline-images))))

    (leaf org-monokakido
      :url ("https://alhassy.github.io/org-special-block-extras/#Links"
            "https://gist.github.com/skoji/936a89f5e1e7c6f93d4a216175408659"))
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
            `((nil . (:maxlevel . 10))))
      (leaf org-refile-source-log
        :disabled t
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
            (setq clavis-org-refile-refiled-from-header nil)))))

    (leaf ob-java
      :custom
      ((org-babel-java-compiler . "javac -encoding UTF-8")))
    (leaf org-eldoc
      :after org
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
               (org-preview-latex-default-process . 'dvisvgm))
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
      (with-eval-after-load 'ox-latex
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
                  (locate-user-emacs-file "lisp/org/ox-latex/templates"))))))

      ;; org-export-latex-no-toc
      (defun org-export-latex-no-toc (depth)
        (when depth
          (format "%% Org-mode is exporting headings to %s levels.\n"
                  depth)))
      (setq org-export-latex-format-toc-function 'org-export-latex-no-toc))
    (setq org-ditaa-jar-path
          "/usr/local/opt/ditaa/libexec/ditaa-0.11.0-standalone.jar")

    (leaf ox-extra
      :after org-contrib
      :require t
      :config
      ;; ignoreタグで見出しを非表示にしつつ内容を表示する
      (ox-extras-activate '(latex-header-blocks ignore-headlines)))
    (leaf org-src-block
      :config
      (defvar-keymap my-org-block-repeat-map
        :repeat t
        "n"   #'org-babel-next-src-block
        "p"   #'org-babel-previous-src-block)
      (add-to-list 'org-src-lang-modes '("json" . js-json))))
  (leaf org-contrib
    :elpaca t
    :after org
    :config
    ;; 有効にする言語 デフォルトでは elisp のみ
    (org-babel-do-load-languages
     'org-babel-load-languages '((C          . t)
                                 (dot        . t)
                                 (emacs-lisp . t)
                                 (go         . t)
                                 (gnuplot    . t)
                                 (java       . t)
                                 (lisp       . t)
                                 (mermaid    . t)
                                 (perl       . t)
                                 (php        . t)
                                 (plantuml   . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (scheme     . t)))
    ;;ob-plantuml
    (add-to-list 'org-babel-default-header-args:plantuml
                 '(:cmdline . "-charset utf-8")))
  (leaf org-keys
    :custom
    (org-use-speed-commands . t)
    :config
    (with-eval-after-load 'org-keys
      (push '("N" . org-next-block) org-babel-key-bindings)
      (push '("P" . org-previous-block) org-babel-key-bindings)))
  (leaf ox-hugo
    :elpaca t
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
                   (function org-hugo-new-subtree-post-capture-template))))
  (leaf org-re-reveal
      :elpaca
      :disabled t
      :after org)
  (leaf org-gcal
    :disabled t
    :elpaca t
    :if (file-exists-p (expand-file-name "org/googlecalendar/org-gcal-config.el" my-share-dir))
    :after org
    :require t
    :custom
    ((org-gcal-down-days . 180)
     (org-gcal-up-days . 180))
    :config
    (load (expand-file-name "org/googlecalendar/org-gcal-config.el" my-share-dir)))
  (leaf anki-editor
    :elpaca t
    :doc "Minor mode for making Anki cards with Org"
    :req "emacs-25" "request-0.3.0" "dash-2.12.0"
    :tag "emacs>=25"
    :emacs>= 25
    :after embark
    :hook
    (anki-editor-mode-hook . (lambda ()
                               (let* ((keymap (copy-keymap embark-region-map)))
                                 (keymap-set keymap  "c"
                                             'my-anki-editor-cloze-region)
                                 (setq-local embark-region-map keymap))))
    :init
    (defun my-anki-editor-cloze-region (_text)
      (call-interactively
       (lambda (&optional arg hint)
         (interactive "NNumber: \nsHint (optional): ")
         (anki-editor-cloze-region arg hint)))))
  (leaf org-pdf*
    :unless (and (eq system-type 'windows-nt)
                 (not (equal (getenv "MSYSTEM") "UCRT64")))
    :config
    (leaf org-pdftools
      :elpaca t
      :after org
      :custom
      `((org-pdftools-root-dir . ,(concat (getenv "HOME") "/GoogleDrive/Books")))
      :hook (org-mode-hook . org-pdftools-setup-link))
    (leaf org-noter
      :elpaca t
      :after (org))
    (leaf org-noter-pdftools
      :elpaca t
      :after (org-noter)
      :require t)
    (leaf pdf-tools
      :elpaca `,@(if (equal (getenv "MSYSTEM") "UCRT64")
                     '(pdf-tools :host github :repo "legends2k/pdf-tools"
                                 :branch "ucrt64"
                                 :files (:defaults "README"
                                           ("build" "Makefile")
                                           ("build" "server"))
                         :remotes ("patch"
                                   ("origin" :host github  :repo "vedang/pdf-tools")))
                    t)
      :mode (("\\.pdf\\'" . pdf-view-mode))
      :hook (pdf-view-mode-hook . (lambda ()
                                    (display-line-numbers-mode 0)))
      :custom ((pdf-view-use-scaling . t)
               (pdf-annot-activate-created-annotations . t)
               (pdf-view-resize-factor . 1.1))
      :bind ((:pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)))
      :require t
      :config
      (pdf-tools-install)))
  (leaf org-download
    :elpaca t
    :after org
    :hook ((org-mode-hook . org-download-enable)))
  (leaf ox-pandoc
      :elpaca t
      :after org
      :if (executable-find "pandoc"))
  (elpaca ox-asciidoc)
  (leaf ox-gfm
      :elpaca t
      :after org)
  (leaf ox-rst
    :elpaca t
    :after (org)
    :custom
    ((org-rst-headline-underline-characters . '(45 126 94 58 39 32 95))))
  (leaf ob-mermaid
    :elpaca t
    :doc "org-babel support for mermaid evaluation"
    :tag "lisp"
    :after org
    :url "https://github.com/arnm/ob-mermaid")
  (leaf org-journal
    :elpaca t
    :commands (org-journal-new-entry org-journal-open-current-journal-file)
    :after org
    ;; :commands org-journal-new-entry
    :custom
    `((org-journal-file-type . 'monthly)
      (org-journal-date-format . "%F (%a)")
      (org-journal-time-format . "<%Y-%m-%d %R> ")
      (org-journal-file-format . "%Y%m.org")
      (org-journal-file-header . "# -*- mode: org-journal; -*-
#+STARTUP: showall"))
    :config
    (defvar-keymap my-org-journal-repeat-map
      :repeat t
      "C-f" #'org-journal-next-entry
      "f"   #'org-journal-next-entry
      "n"   #'org-journal-next-entry
      "C-b" #'org-journal-previous-entry
      "b"   #'org-journal-previous-entry
      "p"   #'org-journal-previous-entry)
    (setq org-journal-dir (expand-file-name  "journal/" org-directory)))
  (leaf org-modern
    :doc "Modern looks for Org"
    :req "emacs-27.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/org-modern"
    :elpaca t
    :emacs>= 27.1
    :after org
    :custom
    (org-modern-star . nil)
    :hook
    ((org-mode-hook . org-modern-mode)
     (org-agenda-finalize-hook . org-modern-agenda))
    :config
    (dolist (face '(org-modern-date-active org-modern-date-inactive))
      (set-face-attribute face nil
                          :family "UDEV Gothic JPDOC"))
    (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))
  (leaf org-modern-indent
    :elpaca (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
    :hook (org-modern-mode-hook . org-modern-indent-mode))

  (elpaca ob-browser)
  (elpaca ox-epub)
  (elpaca ob-go)
  (elpaca ob-php
    (leaf ob-php
      :doc "Execute PHP within org-mode source blocks."
      :req "org-8"
      :tag "php" "babel" "org"
      :url "https://repo.or.cz/ob-php.git"
      :after org))
  (leaf org-latex-impatient
    :elpaca t
    :doc "Preview org-latex Fragments Instantly via MathJax"
    :req "emacs-26" "s-1.8.0" "posframe-0.8.0" "org-9.3" "dash-2.17.0"
    :tag "tools" "tex" "emacs>=26"
    :url "https://github.com/yangsheng6810/org-latex-instant-preview"
    :emacs>= 26
    :hook (org-mode-hook . org-latex-impatient-mode)
    :custom
    (org-latex-impatient-tex2svg-bin . "tex2svg"))
  )
(leaf ox*
  :elpaca (ox-slimhtml :host github :repo "emacsattic/ox-slimhtml")
  :elpaca (ox-tailwind :host github :repo "vascoferreira25/ox-tailwind")
  :custom
  (org-export-allow-bind-keywords . t)
  :config
  (defvar org-export-directory nil
    "org-exportの出力先を指定する変数。buffer-local変数として指定する。")
  (defun org-export-output-file-name--set-directory (orig-fn extension &optional subtreep pub-dir)
    (setq pub-dir (or pub-dir org-export-directory))
    (funcall orig-fn extension subtreep pub-dir))
  (advice-add 'org-export-output-file-name :around 'org-export-output-file-name--set-directory))
(leaf org-roam*
    :elpaca org-roam org-roam-ui consult-org-roam
    :config
    (leaf org-roam
      :req "emacs-26.1" "dash-2.13" "org-9.4" "emacsql-20230228" "magit-section-3.0.0"
      :emacs>= 26.1
      :commands (org-roam-node-find)
      :custom
      `((org-roam-directory . ,(format "%s/roam" org-directory))
        (org-roam-completion-everywhere . t)
        ( org-roam-node-display-template .
          ,(concat "${title:*} "
                   (propertize "[${aliases:10}]" 'face 'font-lock-variable-name-face)
                   " "
                   (propertize "[${tags:10}]" 'face 'org-tag))))
      :bind
      (("C-c n l" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n g" . org-roam-graph)
       ("C-c n i" . org-roam-node-insert)
       ("C-c n c" . org-roam-capture)
       ("C-c n r" . org-roam-node-random)
       ;; Dailies
       ("C-c n j" . org-roam-dailies-capture-today)
       ("C-c n t" . org-roam-dailies-goto-today))
      :config
      (org-roam-db-autosync-mode)
      (when (eq system-type 'darwin)
        (setq org-roam-graph-viewer "open"))
      (setq org-roam-dailies-capture-templates
            '(("d" "default" entry "* %?\n%U\n"
               :target
               (file+head+olp "%<%Y-%m>.org"
                              "#+TITLE: %<%Y-%m>\n\n\n" ("%<%Y-%m-%d>")))))
      (push "ROAM_EXCLUDE" org-default-properties)
      (leaf org-roam-protocol :require t)
      ;; (advice-add 'org-roam-node-open :after (lambda (&rest _) (view-mode)))
      (with-eval-after-load 'cape
        (dolist (f org-roam-completion-functions)
          (advice-add f
                      :around (lambda (oldfn &rest _)
                                (my-cape-wrap-with-annotation oldfn (symbol-name f))))))
      (defvar-keymap my-org-roam-random-repeat-map
        :repeat (:enter (org-roam-node-random))
        "r" #'org-roam-node-random
        "l" #'org-roam-buffer-toggle))
    (leaf org-roam-ui
        :req "emacs-27.1" "org-roam-2.0.0" "simple-httpd-20191103.1446" "websocket-1.13"
        :emacs>= 27.1
        :after org-roam
        :custom ((org-roam-ui-sync-theme . t)
                 (org-roam-ui-follow . t)
                 (org-roam-ui-update-on-save . t)))
    (leaf consult-org-roam
        :doc "Consult integration for org-roam"
        :req "emacs-27.1" "org-roam-2.2.0" "consult-0.16"
        :tag "emacs>=27.1"
        :url "https://github.com/jgru/consult-org-roam"
        :emacs>= 27.1
        :after org-roam consult))
(leaf org-tag-beautify
  :disabled t
  :elpaca (org-tag-beautify :host github :repo "emacsmirror/org-tag-beautify" :branch "master")
  :doc "Beautify Org mode tags"
  :req "emacs-26.1" "org-pretty-tags-0.2.2" "all-the-icons-5.0.0"
  :url "https://repo.or.cz/org-tag-beautify.git"
  :emacs>= 26.1
  :after org
  :custom
  `(org-tag-beautify-data-dir . ,(format "%sorg-tag-beautify/data/" elpaca-repos-directory))
  :config
  (org-tag-beautify-mode 1))
(leaf anki-editor-org-src
  :after org
  ;; :leaf-defer nil
  :hook
  (org-src-mode-hook . (lambda ()
                         (if (string-match (regexp-quote "[ anki-editor ]") (buffer-name))
                             (anki-editor-mode))))
  :init
  (add-to-list 'org-src-lang-modes '("anki-editor" . org)))

(leaf mermaid-mode
  :elpaca t
  :config
  (my-org-push-src-lang-modes mermaid))
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
    (setq org-mu4e-link-query-in-headers-mode nil)))

(leaf yatex
    :elpaca (yatex :repo "https://www.yatex.org/gitbucket/git/yuuji/yatex.git")
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
;; for yatex
(when (equal system-type 'darwin)
  (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:/Applications/Skim.app/Contents/SharedSupport:$PATH" t)
  (setq exec-path (append '("/usr/local/bin" "/Library/TeX/texbin" "/Applications/Skim.app/Contents/SharedSupport") exec-path)))
(leaf php-mode
  :elpaca t
  :custom
  ((php-manual-url . 'ja)))
(elpaca ac-php)
(leaf flycheck-phpstan
  :elpaca t
  :hook (php-mode-hook . (lambda ()
                           (require 'flycheck-phpstan)
                           (flycheck-mode t))))
(leaf company-php
    :elpaca t
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
(leaf typescript-mode
    :elpaca t
    :hook ((typescript-mode-hook . lsp-deferred)
           (typescript-mode-hook . electric-pair-local-mode)))
(leaf json-mode
  :hook ((js-json-mode-hook . electric-pair-local-mode)))
(elpaca rainbow-mode)
(elpaca poetry)
(leaf pipenv
  :elpaca t
  :disabled t
  :hook (python-mode-hook . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))
(elpaca hydra)
(leaf go-mode
  :elpaca t
  :hook ((go-mode-hook . lsp-deferred))
  :config
  (my-org-push-src-lang-modes go))
(leaf go-ts-mode
  :hook ((go-ts-mode-hook . lsp-deferred)
         (go-ts-mode-hook . (lambda ()
                              (add-hook 'before-save-hook #'go-ts-mode-gofmt-before-save nil t)))
         (go-ts-mode-hook . electric-pair-local-mode))
  :init
  (push '(go-mode . go-ts-mode) major-mode-remap-alist)
  (defun go-ts-mode-gofmt-before-save ()
    (interactive)
    (when (eq major-mode 'go-ts-mode)
      (require 'go-mode)
      (gofmt))))
(leaf csharp-ts-mode
  :hook (csharp-ts-mode-hook . lsp-deferred)
  :init
  (push '(csharp-mode . csharp-ts-mode) major-mode-remap-alist))
(leaf js-ts-mode
  :disabled t
  :hook (js-ts-mode-hook . lsp-deferred)
  :init
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist))
(leaf toml
  :config
  (my-org-push-src-lang-modes conf-toml toml))
(when (version< emacs-version "29")
  (elpaca csharp-mode))
(elpaca android-mode)
(leaf swift
  :when (eq system-type 'darwin)
  :elpaca swift-mode
  :elpaca (lsp-sourcekit :host github :repo "emacs-lsp/lsp-sourcekit"
                         :files (:defaults "lsp-sourcekit.el"))
  :custom
  `(lsp-sourcekit-executable . ,(string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))
(leaf applescript-mode
  :elpaca t
  :init
  (my-org-push-src-lang-modes applescript))
(leaf ccls
  :elpaca t
  :after lsp-mode
  :require t
  :hook ((c-mode-hook c++-mode-hook objc-mode-hook) .
         (lambda ()
           (smartparens-mode -1)
           (electric-pair-local-mode 1)))
  :custom
  `((ccls-executable . ,(or (executable-find "ccls")
                            (executable-find "ccls-clang-11")))))
(leaf smartparens
  :elpaca t
  :diminish t
  :require smartparens-config
  :hook (emacs-lisp-mode-hook . smartparens-mode)
  :bind
  (:emacs-lisp-mode-map
   ("C-c C-u" . sp-backward-up-sexp)
   ("C-c C-n" . sp-next-sexp)
   ("C-c C-p" . sp-previous-sexp))
  :config
  (defvar-keymap my-sp-move-repeat-map
    :repeat t
    "u"   #'sp-backward-up-sexp
    "n"   #'sp-next-sexp
    "p"   #'sp-previous-sexp
    "@"   #'sp-mark-sexp
    ))
(leaf kotlin-mode
  :elpaca t
  :mode (("\\.kt\\'" . kotlin-mode))
  :config
  (my-org-push-src-lang-modes kotlin))
(elpaca ob-kotlin)
(leaf dart-mode
  :elpaca (dart-mode :host github :repo "bradyt/dart-mode")
  :doc "Major mode for editing Dart files"
  :req "emacs-24.3"
  :tag "languages" "emacs>=24.3"
  :url "https://github.com/bradyt/dart-mode"
  :mode (("\\.dart\\'" . dart-mode))
  :hook ((dart-mode-hook . lsp-deferred)
         (dart-mode-hook . (lambda ()
                             (electric-pair-local-mode 1)
                             (when (featurep 'embark)
                               (setq-local embark-target-finders
                                           (append (remove
                                                    'embark-target-file-at-point
                                                    embark-target-finders)
                                                   '(embark-target-file-at-point))))))))
(leaf zig-mode
  :elpaca t)
(elpaca fsharp-mode)
(leaf plantuml-mode
  :elpaca t
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
(elpaca htmlize)
(leaf adoc-mode
  :elpaca t
  :bind
  (:adoc-mode-map
   ("C-c C-n" . outline-next-visible-heading)
   ("C-c C-p" . outline-previous-visible-heading)))
(elpaca pandoc)
(elpaca graphviz-dot-mode)
(leaf editorconfig
  :elpaca `(,@(version< emacs-version "30"))
  :diminish editorconfig-mode
  :global-minor-mode t)
(leaf easy-hugo
  :disabled t
  :custom
  ((easy-hugo-org-header . t)
   (easy-hugo-default-ext . ".org")))
(elpaca npm-mode)
(elpaca autodisass-java-bytecode)
(elpaca solarized-theme)
(leaf modus-themes
  :elpaca t
  :require t
  :custom
  ((modus-themes-italic-constructs . t)
   (modus-themes-org-blocks . 'gray-background)
   (modus-themes-custom-auto-reload . t)
   (modus-themes-disable-other-themes . t))
  :preface
  (defcustom my-theme-color 'light
    "Choose between 'dark' atd 'light' themes."
    :type '(choice (const :tag "Light" light)
                   (const :tag "Dark" dark)
                   (const :tag "Auto(emacs-plus only)" auto))
    :group 'my-custom-settings)
  :config
  (defvar modus-themes-item-pairs
    (if (eq (nth 1 modus-themes-items) 'modus-vivendi)
        (cl-labels
            ((twochunk (x)
               (if (null (cadr x))
                   (car x)
                 (cons (list (car x) (cadr x)) (twochunk (cddr x))))))
          (twochunk modus-themes-items))
      (mapcar (lambda (n)
                (list (nth n modus-themes-items)
                      (nth (+ n 4) modus-themes-items)))
              (number-sequence 0 3)))
    "list of `modus-themes-items' pair")
  (setq modus-themes-to-toggle (nth 3   ;(random (length modus-themes-item-pairs))
                                    modus-themes-item-pairs))
  (defun my-modus-themes--change-appearance (&optional appearance)
    (when (null appearance)
      (setq appearance (cond ((eq 'auto my-theme-color)
                              (if (boundp 'ns-system-appearance)
                                  ns-system-appearance
                                'light))
                             (t my-theme-color))))
    (modus-themes-load-theme
     (nth (if (eq appearance 'dark) 1 0) modus-themes-to-toggle)))
  ;; (add-hook 'ns-system-appearance-change-functions
  ;;           #'(lambda (appearance)
  ;;               (if
  ;;                   (and custom-enabled-themes
  ;;                        (string-match-p "modus-"
  ;;                                        (symbol-name
  ;;                                         (car custom-enabled-themes))))
  ;;                   (my-modus-themes--change-appearance appearance))))
  ;; (my-modus-themes--change-appearance)
  )
(leaf circadian
  :elpaca t
  :after modus-themes
  :config
  (setq circadian-themes `((:sunrise . ,(nth 0 modus-themes-to-toggle))
                           ("7:30"   . ,(nth 0 modus-themes-to-toggle))
                           (:sunset  . ,(nth 1 modus-themes-to-toggle))
                           ("18:00"  . ,(nth 1 modus-themes-to-toggle))))
  (circadian-setup))
(leaf doom-themes :elpaca (doom-themes :repo ("doomemacs/themes"  . "doom-themes")))
(leaf markdown-mode
  :elpaca t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom ((markdown-command . "multimarkdown"))
  :hook (markdown-mode-hook . (lambda ()
                                (setq-local imenu-create-index-function
                                            'markdown-imenu-create-flat-index)))
  :config
  (when (member "IPAGothic" (font-family-list))
    (set-face-attribute 'markdown-table-face nil
                        :family "IPAGothic"))
  (defvar-keymap my-markdown-navigation-repeat-map
    :repeat t
    "n" #'markdown-outline-next
    "p" #'markdown-outline-previous
    "TAB" #'markdown-cycle
    "u" #'markdown-up-heading)
  (my-org-push-src-lang-modes markdown))
(elpaca docker)

(elpaca docker-compose-mode)
(leaf review-mode
  :elpaca t
  :mode (("\\.re\\'" . review-mode)))
(elpaca csv-mode)

(elpaca gnuplot)

(leaf gdb
  ;;; GDB 関連
  :hook
  (;; 変数の上にマウスカーソルを置くと値を表示
   (gdb-mode-hook . (lambda () (gud-tooltip-mode t))))
  :custom
  `((gdb-many-windows . t)              ; 有用なバッファを開くモード
    ;;; I/O バッファを表示
    (gdb-use-separate-io-buffer . t)
    ;;; t にすると mini buffer に値が表示される
    (gud-tooltip-echo-area . nil)
    ;;; バックアップファイルを作成しない
    (make-backup-files .t)))

(elpaca ssh-config-mode)

(leaf fish-mode
  :elpaca t
  :config
  (my-org-push-src-lang-modes fish))

(leaf dockerfile-mode
  :elpaca t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(elpaca meson-mode)
(leaf git-modes
    :elpaca t
    :require t)
(elpaca groovy-mode)
(leaf server
  :commands (server-running-p)
  :hook
  (emacs-startup-hook . (lambda ()
                          (unless (server-running-p)
                            (server-start)))))

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

(leaf ispell
  :require t
  :config
  ;; (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))
(leaf epg-config
  :custom
  (epg-pinentry-mode . 'loopback))
(leaf vimrc-mode
    :elpaca t
    :mode
    ("\\.vim\\(rc\\)?\\'" . vimrc-mode))
(leaf lsp-bridge
  :elpaca (lsp-bridge :host github :repo "manateelazycat/lsp-bridge"
                      :files (:defaults "lsp_bridge.py" "acm/*" "core"
                                        "langserver" "multiserver" "resources"))
  :disabled t
  :bind
  (:lsp-bridge-mode-map
   ("C-c C-l a a" . lsp-bridge-code-action)))
(elpaca flycheck-eglot)
(elpaca tree-sitter-langs)
(elpaca treesit-auto)
(leaf sideline
  :elpaca t sideline-flymake sideline-lsp
  :custom
  ((sideline-flymake-display-mode . 'point)
   (sideline-backends-right . '((sideline-lsp      . up)
                                (sideline-flymake . down)))))
(leaf eglot-java
  :elpaca t
  :doc "Java extension for the eglot LSP client"
  :req "emacs-26.1" "eglot-1.0" "jsonrpc-1.0.0"
  :tag "languages" "convenience" "emacs>=26.1"
  :url "https://github.com/yveszoundi/eglot-java"
  :emacs>= 26.1
  :custom
  ((eglot-java-prefix-key . "C-c C-l")))
(leaf consult-eglot
  :elpaca t
  :doc "A consulting-read interface for eglot"
  :req "emacs-27.1" "eglot-1.7" "consult-0.9"
  :tag "lsp" "completion" "tools" "emacs>=27.1"
  :url "https://github.com/mohkale/consult-eglot"
  :emacs>= 27.1
  :after eglot consult)
(elpaca vterm)
(leaf elfeed
  :elpaca (elfeed :files (:defaults "web/*"))
  :doc "an Emacs Atom/RSS feed reader"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :url "https://github.com/skeeto/elfeed"
  :commands (elfeed)
  :emacs>= 24.3
  :bind ((:elfeed-show-mode-map
          ("S-SPC" . scroll-down-command))
         (:elfeed-search-mode-map
          ("j" . forward-line)
          ("n" . forward-line)
          ("k" . previous-line)
          ("p" . previous-line)
          ("e" . (lambda () (interactive)(eww (my-elfeed-yank-entry-url))))
          ("a" . my-elfeed-safari-add-reading-item)
          ("s" . my-elfeed-search-live-filter))
         (:elfeed-show-mode-map
          ("a" . my-elfeed-safari-add-reading-item)))
  :init
  (defvar-keymap my-elfeed-yank-map
    "y" #'elfeed-search-yank
    "m" #'my-elfeed-search-yank-markdown
    "o" #'my-elfeed-search-yank-org)
  :custom
  ((elfeed-search-date-format . '("%Y-%m-%d %H:%M" 16 :left))
   (elfeed-search-filter . "@24-hours-ago +unread"))
  :config
  ;;osascript -e 'tell application "Safari" to add reading list item "http://totl.net/"'
  (defun my-elfeed-safari-add-reading-item ()
    (interactive)
    (unless (and (eq system-type 'darwin)
                 (not (equal (shell-command-to-string "command -v osascript") "")))
      (error "not found 'osascript' command"))
    (let ((url (car (mapcar #'elfeed-entry-link (elfeed-search-selected)))))
      (require 'ffap)
      (if (not (ffap-url-p url))
          (error "invalid URL: %s" url)
        (call-process-shell-command
         (format "osascript -e 'tell application \"Safari\" to add reading list item \"%s\"'" (my-elfeed-yank-entry-url)))
        (message "The selected entry is added to Safari's reading list.")
        (elfeed-search-untag-all-unread))))
  (defun my-elfeed-yank-entry-url ()
    (interactive)
    (let ((url (car (mapcar #'elfeed-entry-link (elfeed-search-selected)))))
      (if (equal url "")
          (error "Selected entry's url is empty")
        url)))
  (defun my-elfeed-search-live-filter ()
    (interactive)
    (unwind-protect (let* ((elfeed-search-filter-active :live)
                           (input (my-elfeed--tag-completing-multi)))
                      (unless (string-blank-p input)
                        (setq elfeed-search-filter input)
                        (elfeed-search-update :force)))))
  (defun my-elfeed--tag-completing-multi ()
    (require 'elfeed)
    (let* ((all-tags (elfeed-db-get-all-tags))
           (selected-tags (completing-read-multiple "Filter: "
                                                    all-tags
                                                    nil nil
                                                    elfeed-search-filter))
           (trimmed-tags (seq-remove #'string-blank-p
                                     (mapcar #'string-trim selected-tags)))
           (formatted-tags (mapcar (lambda (tag)
                                     (cond ((member (aref tag 0) '(?@ ?+)) tag)
                                           (t (format "+%s" tag))))
                                   trimmed-tags)))
      (string-join formatted-tags " ")))
  (defun my-elfeed-search-set-filter (old-fn &rest args)
    (interactive)
    (funcall old-fn (my-elfeed--tag-completing-multi)))
  (advice-add 'elfeed-search-set-filter :around #'my-elfeed-search-set-filter)
  (defun my-elfeed-search-yank-markdown ()
    (interactive)
    (my-elfeed-search-yank 'my-markdown--generate-link))
  (defun my-elfeed-search-yank-org ()
    (interactive)
    (my-elfeed-search-yank 'my-org--generate-link))
  (defun my-elfeed-search-yank (yank-function)
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (links (mapcar #'elfeed-entry-link entries))
           (titles (mapcar #'elfeed-entry-title entries))
           (markdowns (mapconcat
                       #'identity
                       (cl-mapcar
                        yank-function
                        titles links) "\n")))
      (kill-new markdowns)
      (message "Copied:%s" markdowns)))
  (leaf-keys-bind-keymap
   ((elfeed-search-mode-map :package elfeed
                            ("y" . my-elfeed-yank-map)))))
(leaf elfeed-goodies
  :elpaca t
  :doc "Elfeed goodies"
  :req "popwin-1.0.0" "powerline-2.2" "elfeed-2.0.0" "cl-lib-0.5" "link-hint-0.1"
  :url "https://github.com/algernon/elfeed-goodies"
  :require  popwin
  :after elfeed
  :bind (:elfeed-search-mode-map
         :package (elfeed elfeed-goodies)
         ("l" . elfeed-goodies/toggle-logs))
  :custom
  ((elfeed-goodies/entry-pane-position . 'bottom))
  :init
  (elfeed-goodies/setup))

(leaf powershell
  :elpaca t
  :config
  (my-org-push-src-lang-modes powershell)
  (my-org-push-src-lang-modes powershell pwsh))

(elpaca lua-mode)
(leaf pcre2el
  :elpaca t
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
(leaf regex-tool
  :elpaca t
  :doc "A regular expression evaluation tool for programmers"
  :tag "development" "programming" "languages" "regex"
  :url "http://www.newartisans.com/"
  :custom
  (regex-tool-backend . 'perl)
  :bind
  (:regex-tool-mode-map
   ("C-c C-q" . regex-tool-quit)))
(leaf visual-regexp-steroids
  :elpaca t
  :doc "Extends visual-regexp to support other regexp engines"
  :req "visual-regexp-1.1"
  :tag "feedback" "visual" "python" "replace" "regexp" "foreign" "external"
  :url "https://github.com/benma/visual-regexp-steroids.el/")

(leaf repeat-mode
  :emacs>= 28
  :custom
  (repeat-exit-key . "q")
  :global-minor-mode repeat-mode)

(leaf grip-mode
  :elpaca t
  :doc "Instant GitHub-flavored Markdown/Org preview using grip."
  :bind ((:markdown-mode-command-map
          ("g" . grip-mode))))
(elpaca dirvish)
(leaf shrface
  :elpaca t
  :doc "Extend shr/eww with org features and analysis capability"
  :req "emacs-25.1" "org-9.0" "language-detection-0.1.0"
  :tag "faces" "emacs>=25.1"
  :url "https://github.com/chenyanming/shrface"
  :emacs>= 25.1
  :hook (;; (eww-after-render-hook . shrface-mode)
         (nov-mode-hook . shrface-mode))
  :require t
  :custom ((shrface-href-versatile . t))
  :custom-face
  (shr-text . '((t (:family "UDEV Gothic JPDOC" :inherit (variable-pitch-text)))))
  :bind
  (:nov-mode-map
   :package nov
   ("n" . org-next-visible-heading)
   ("p" . org-previous-visible-heading)
   ("s" . org-toggle-narrow-to-subtree)
   ("u" . outline-up-heading))
  :config
  (shrface-default-keybindings) ; setup default keybindings
  (with-eval-after-load 'nov
    (setq nov-shr-rendering-functions (append '((img . nov-render-img)
                                                (title . nov-render-title))
                                              shr-external-rendering-functions))))
(leaf nov
  :elpaca t
  :doc "Featureful EPUB reader mode"
  :req "esxml-0.3.6" "emacs-25.1"
  :tag "epub" "multimedia" "hypermedia" "emacs>=25.1"
  :url "https://depp.brause.cc/nov.el"
  :emacs>= 25.1
  :custom
  ((nov-text-width . t))
  :preface
  (defun quit-window-confirm ()
    "Ask for confirmation before quitting the window."
    (interactive)
    (when (y-or-n-p "Are you sure you want to this window? ")
      (quit-window)))
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode-hook . (lambda () (visual-line-mode 1)))
  :bind (:nov-mode-map
         ("q" . quit-window-confirm)))
(leaf nov-xwidget
  :elpaca (nov-xwidget :host github :repo "chenyanming/nov-xwidget")
  :disabled t
  :after nov
  :commands nov-xwidget-inject-all-files
  :bind
  (:nov-mode-map
   ("o" . nov-xwidget-view))
  :hook
  (nov-mode-hook . nov-xwidget-inject-all-files))
(leaf speed-type
  :elpaca (speed-type :ref "b982ee6" :pin t)
  :doc "Practice touch and speed typing"
  :req "emacs-25.1"
  :tag "games" "emacs>=25.1"
  :url "https://github.com/parkouss/speed-type"
  :emacs>= 25.1
  :custom-face (speed-type-default . '((t (:height 1.2))))
  :config
  (with-eval-after-load 'corfu
    (add-hook 'speed-type-mode-hook
              (lambda () (corfu-mode -1)))))
(elpaca suggest)
(leaf emacs-eat
  :unless (eq 'windows-nt system-type)
  :elpaca (emacs-eat :type git
                     :host codeberg
                     :repo "akib/emacs-eat"
                     :main "eat.el"
                     :files ("*.el" ("term" "term/*.el") "*.texi"
                             "*.ti" ("terminfo/e" "terminfo/e/*")
                             ("terminfo/65" "terminfo/65/*")
                             ("integration" "integration/*")
                             (:exclude ".dir-locals.el" "*-tests.el")))
  :bind ((:projectile-command-map
          :package projectile
          ("x a" . eat-project)
          ("x 4 a" . eat-project-other-window))
         (:project-prefix-map
          :package project
          ("s" . nil)
          ("s s" . project-shell)
          ("s a" . eat-project)))
  :init
  (with-eval-after-load 'projectile
    (def-projectile-commander-method ?x "Open EAT buffer." (eat-project))))
(leaf claude-code-ide
  :elpaca (claude-code-ide :type git :host github :repo "manzaltu/claude-code-ide.el")
  ;; :bind
  ;; ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))
(elpaca jinx)
(leaf dmacro
  :elpaca t
  :global-minor-mode global-dmacro-mode)
(elpaca puni)
(elpaca rg)
(elpaca mistty)
(elpaca syncthing)
(elpaca bnf-mode)
(elpaca (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))
(leaf indent-bars
  :elpaca (indent-bars :type git :host github :repo "jdtsmith/indent-bars"))
(leaf info-downloader
  :elpaca (info-downloader :type git :host github :repo "furusi/info-downloader")
  :after info)
(leaf ellama
  :if (executable-find "ollama")
  :elpaca t
  :custom
  ((ellama-language . "Japanese"))
  :bind
  (("C-c e" . ellama-transient-main-menu)
   (:embark-region-map
    :package embark
    ("C-t" . ellama-translate)))
  :config
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model "llama3.1:8b"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-summarization-provider
	  (make-llm-ollama
	   :chat-model "qwen2.5:3b"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
	  (make-llm-ollama
	   :chat-model "qwen2.5-coder:7b"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
	  '(("zephyr" . (make-llm-ollama
			 :chat-model "zephyr:7b-beta-q6_K"
			 :embedding-model "zephyr:7b-beta-q6_K"))
	    ("mistral" . (make-llm-ollama
			  :chat-model "mistral:7b-instruct-v0.2-q6_K"
			  :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
	    ("mixtral" . (make-llm-ollama
			  :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
			  :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	  (make-llm-ollama
	   :chat-model "llama3.1:8b"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("stop" . ["\n"]))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider
	  (make-llm-ollama
	   :chat-model "qwen2.5:7b"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params
	   '(("num_ctx" . 32768))))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; send last message in chat buffer with C-c C-c
  (add-hook 'org-ctrl-c-ctrl-c-final-hook #'ellama-chat-send-last-message))
(leaf chatgpt-shell
  :elpaca t)
(leaf infinite-scroll
  :elpaca (infinite-scroll :type git :host github :repo "zonuexe/infinite-scroll.el"))
(leaf ultra-scroll
  :unless (eq system-type 'windows-nt)
  :elpaca (ultra-scroll  :type git :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))
(leaf emacs-everywhere
  :if (member system-type '(darwin gnu/linux))
  :elpaca t)
(leaf verb
  :after org
  :elpaca t
  :bind-keymap (:org-mode-map :package org ("C-c C-r" . verb-command-map))
  :config
  (push '("verb" . ?v) org-tag-alist))

(add-to-list 'load-path (expand-file-name (locate-user-emacs-file "lisp")))
(require 'my-lisp nil t)
(require 'my-window nil t)

(let ((f (expand-file-name ".config/emacs/config.el" my-share-dir)))
  (when (file-exists-p f)
    (load-file f)))

;; (profiler-report)
;; (profiler-stop)

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'magit-diff-edit-hunk-commit 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
