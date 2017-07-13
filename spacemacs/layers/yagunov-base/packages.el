;;; packages.el --- yagunov-base Layer packages file for Spacemacs
;;
;; Copyright (c) 2015, 2016, 2017 Andrey Yagunov
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

        ;; NB: Use evil-exchange instead (gx)
        ;; anchored-transpose
        ;; second-sel

        ;; google-translate

        ;;ergoemacs-mode

        ;; highlight-symbol

        ;; magit-gitflow

        ;; cc-mode

        ;; (ox-reveal :location (recipe :fetcher github :repo "yjwen/org-reveal"))

        persistent-scratch
        evil-lion
        pomidor
        switch-window
        ))

(defun yagunov-base/init-persistent-scratch ()
  (use-package persistent-scratch
    :config
    (persistent-scratch-setup-default)))

(defun yagunov-base/init-evil-lion ()
  (use-package evil-lion
    :ensure t
    :config (evil-lion-mode)))

(defun yagunov-base/init-pomidor ()
  (use-package pomidor
    :config (spacemacs/set-leader-keys
              "o p" 'pomidor)))

(defun yagunov-base/init-switch-window ()
  (use-package switch-window
    :ensure t
    :init
    (progn
      (message "switch-window initialization")
      (setq switch-window-shortcut-style 'qwerty
            switch-window-minibuffer-shortcut nil
            switch-window-qwerty-shortcuts
            '("a" "s" "d" "f" "j" "k" "l" ":" "w" "e" "i" "o" "g" "h" "r" "q" "u" "v" "n")))))


;; TODO: Find better place for this!
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (interactive) (semantic-mode -1)))


;; (defun yagunov-base/init-accelerate ()
;;   (use-package accelerate
;;     :config
;;     (progn
;;       (accelerate previous-line 5)
;;       (accelerate next-line 5)
;;       (accelerate backward-char 3)
;;       (accelerate forward-char 3)
;;       (accelerate dired-previous-line 4)
;;       (accelerate dired-next-line 4))))

;; (defun yagunov-base/init-anchored-transpose ()
;;   (use-package anchored-transpose
;;     :config (define-key evil-visual-state-map (kbd "C-t") 'yagunov/smart-transpose)))

;; (defun yagunov-base/init-second-sel ()
;;   (use-package second-sel
;;     :config
;;     (spacemacs/set-leader-keys
;;       "SPC" 'yagunov/set-mark-command
;;       "y"   'secondary-dwim)))


;; (defun yagunov-base/init-ergoemacs-mode ()
;;   (use-package ergoemacs-mode
;;     :config (progn
;;               (global-set-key (kbd "C-a") 'ergoemacs-beginning-of-line-or-what)
;;               (global-set-key (kbd "C-e") 'ergoemacs-end-of-line-or-what))))

;; TODO: Learn how to use 'M-m s h'
;; (defun yagunov-base/init-highlight-symbol ()
;;   (use-package highlight-symbol
;;     :bind (("C-c m"   . highlight-symbol-at-point)
;;            ("C-c M"   . highlight-symbol-remove-all)
;;            ("C-c M-m" . highlight-symbol-remove-all)
;;            ("C-c C-r" . highlight-symbol-query-replace)
;;            )
;;     :config
;;     (progn
;;       (setq highlight-symbol-colors
;;             '("orange" "brown" "dark cyan" "MediumPurple1" "dark green"
;;               "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab")))))



;; (defun yagunov-base/init-ox-reveal ()
;;   (load-library "ox-reveal"))

