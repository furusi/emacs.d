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

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-enable-at-startup nil)

;; Language and Character Code
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

(setq default-frame-alist '((width . 180) (height . 40) (top . 1) (left . 1) (tool-bar-lines . 0) (menu-bar-lines . 0)))
