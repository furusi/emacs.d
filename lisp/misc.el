(defun straight-update-repositories ()
  "melpa等のレポジトリとstraight, use-packageをpullする。"
  (interactive)
  (dolist (package
            '("melpa" "gnu-elpa-mirror"
              "emacsmirror-mirror" "straight" "use-package"))
      (straight-pull-package-and-deps package)))
