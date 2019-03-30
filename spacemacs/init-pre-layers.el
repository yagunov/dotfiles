;;; init-pre-layers.el --- User configuration loaded before Spacemacs loads any layers.
;;
;; Copyright (c) 2019 Andrey Yagunov
;;
;; Author: Andrey Yagunov <yagunov86@gmail.com>
;; URL: https://github.com/yagunov/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: WTFPL

;;; Commentary:

;;; Code:


(defun yagunov/pre-layers-setup ()
  "Early stage user setup."

  ;; Add custom prefixes to nameless-mode
  (spacemacs|use-package-add-hook nameless
    :post-config
    (add-to-list 'nameless-global-aliases '("Y" . "yagunov")))

  ;; Configure default languages for Google translate
  (spacemacs|use-package-add-hook google-translate
    :post-config
    (setq google-translate-enable-ido-completion t
          google-translate-default-source-language "auto"
          google-translate-default-target-language "en"))

  ;; Git UI configuration
  (spacemacs|use-package-add-hook magit
    :post-config
    (setq magit-push-always-verify nil  ; Use default branch on 'PP'
          magit-diff-refine-hunk t
          magit-diff-options '("--ignore-space-change")))

  ;; Use faster find replacement when possible.
  (spacemacs|use-package-add-hook projectile
    :post-config
    (when (executable-find "fd")
      (setq projectile-generic-command "fd . --type=file --print0"))))


;;; init-pre-layers.el ends here
