;;; packages.el --- yagunov-base Layer packages File for Spacemacs
;;
;; Copyright (c) 2015, 2016 Andrey Yagunov
;;
;; Author: Andrey Yagunov <yagunov86@gmail.com>
;; URL: <TODO>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq yagunov-base-packages
      '(
        ;; (accelerate :location (recipe :fetcher github :repo "yagunov/accelerate.el"))

        ;; anchored-transpose
        ;; second-sel

        google-translate

        ;;ergoemacs-mode

        highlight-symbol

        magit-gitflow
        ))


;; TODO: Find better place for this!
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (interactive) (semantic-mode -1)))

(setq prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode)


(defun yagunov-base/init-accelerate ()
  (use-package accelerate
    :config
    (progn
      (accelerate previous-line 5)
      (accelerate next-line 5)
      (accelerate backward-char 3)
      (accelerate forward-char 3)
      (accelerate dired-previous-line 4)
      (accelerate dired-next-line 4))))

(defun yagunov-base/init-anchored-transpose ()
  (use-package anchored-transpose
    :config (global-set-key (kbd "C-t") 'yagunov/smart-transpose)))

(defun yagunov-base/init-second-sel ()
  (use-package second-sel
    :config
    (progn
      (global-set-key (kbd "C-SPC") 'yagunov/set-mark-command)
      (global-set-key (kbd "C-M-y") 'secondary-dwim))))

(defun yagunov-base/init-google-translate ()
  (use-package google-translate
    :demand t
    :config
    (progn
      (setq google-translate-enable-ido-completion t
            google-translate-default-source-language "auto"
            google-translate-default-target-language "en")
      (global-set-key (kbd "C-x t") 'yagunov/google-translate))))

;; (defun yagunov-base/init-ergoemacs-mode ()
;;   (use-package ergoemacs-mode
;;     :config (progn
;;               (global-set-key (kbd "C-a") 'ergoemacs-beginning-of-line-or-what)
;;               (global-set-key (kbd "C-e") 'ergoemacs-end-of-line-or-what))))

;; TODO: Learn how to use 'M-m s h'
(global-set-key (kbd "M-n") 'ahs-forward)
(global-set-key (kbd "M-p") 'ahs-backward)
(defun yagunov-base/init-highlight-symbol ()
  (use-package highlight-symbol
    :bind (("C-c m"   . highlight-symbol-at-point)
           ("C-c M"   . highlight-symbol-remove-all)
           ("C-c M-m" . highlight-symbol-remove-all)
           ("C-c C-r" . highlight-symbol-query-replace)
           ;; ("M-n"     . highlight-symbol-next)
           ;; ("M-p"     . highlight-symbol-prev)
           )
    :config
    (progn
      (setq highlight-symbol-colors
            '("orange" "brown" "dark cyan" "MediumPurple1" "dark green"
              "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab")))))

(defun yagunov-base/init-magit-gitflow ()
  (use-package magit-gitflow
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))
;; Use default branch on 'PP'.
(setq magit-push-always-verify nil)     ;TODO: Find better place for this
