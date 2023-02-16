(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my-saved-file-name-handler-alist)))

(defconst my-default-gc-cons-threshold (* 1024 1024 16))
(setq gc-cons-threshold most-positive-fixnum)
;; Run GC every 60 seconds if emacs is idle.
(run-with-idle-timer 60.0 t #'garbage-collect)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my-default-gc-cons-threshold)))

(setq system-time-locale "C")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(defvar bootstrap-version)
(custom-set-variables `(package-user-dir
                        ,(locate-user-emacs-file
                          (format "elpa/%s" emacs-version))))
(setq straight-base-dir
      (locate-user-emacs-file
        (format "packages/%s/" emacs-version)))
(setq straight-profiles
      `((nil . ,(locate-user-emacs-file
                 "straight/versions/default.el"))))

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(custom-set-variables '(straight-vc-git-default-clone-depth 150))

;;elpaca
(defvar elpaca-directory (format "%spackages/%s/elpaca/" (expand-file-name user-emacs-directory) emacs-version))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--")))))
          (progn
            (byte-recompile-directory repo 0 'force)
            (require 'elpaca)
            (and (fboundp 'elpaca-generate-autoloads)
                 (elpaca-generate-autoloads "elpaca" repo))
            (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca use-package (require 'use-package))
(setq package-enable-at-startup nil)
;;elpaca end

;; Language and Character Code
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (straight-use-package 'hydra)
    (straight-use-package 'el-get)
    (straight-use-package 'blackout)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))
  
  (leaf leaf-convert
    :doc "Convert many format to leaf format"
    :req "emacs-26.1" "leaf-3.6.0" "leaf-keywords-1.1.0" "ppp-2.1"
    :tag "tools" "emacs>=26.1"
    :url "https://github.com/conao3/leaf-convert.el"
    :emacs>= 26.1
    :straight t
    :after leaf leaf-keywords ppp)
  )

(leaf diminish
  :straight t
  :require t
  :diminish (show-paren-mode))

(leaf feather
  :ensure t
  :diminish feather-mode
  :config (feather-mode))

;; </leaf-install-code>
