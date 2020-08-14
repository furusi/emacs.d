(setq system-time-locale "C")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(defvar bootstrap-version)
(setq straight-base-dir (concat user-emacs-directory "packages/" emacs-version "/"))
(setq straight-profiles (list (cons nil (concat user-emacs-directory "straight/versions/default.el"))))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
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
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Language and Character Code
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

