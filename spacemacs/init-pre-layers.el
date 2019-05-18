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

  ;; Git UI configuration
  (spacemacs|use-package-add-hook magit
    :post-config
    (setq magit-push-always-verify nil  ; Use default branch on 'PP'
          magit-diff-refine-hunk t
          magit-diff-options '("--ignore-space-change")))

  ;; Use faster find replacement when possible.
  (when (executable-find "fd")
    (spacemacs|use-package-add-hook projectile
      :post-config
      (setq projectile-generic-command "fd . --type=file --print0"))))


;;; init-pre-layers.el ends here
