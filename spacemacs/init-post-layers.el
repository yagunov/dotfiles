;;; init-post-layers.el --- User configuration loaded after Spacemacs loads all layers.
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

(require 'time-stamp)
(require 'copyright)


(defun yagunov/post-layers-setup ()
  "Last stage user setup."

  ;; Personal information:
  (setq user-full-name "Andrey Yagunov"
        user-mail-address "yagunov86@gmail.com")

  ;; Some editing preferences:
  (setq-default tab-width 4)

  ;; Set language environment and coding system
  (set-language-environment "Russian")
  (set-default-coding-systems 'utf-8)
  (set-coding-system-priority 'utf-8 'cp1251 'koi8-r 'cp866)

  ;; Enable structurally safe Lisp editing
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)

  ;; Current line highlighting makes it less visible in some dark color themes
  (global-hl-line-mode -1)

  ;; Update file timestamps in headers
  (add-hook 'before-save-hook '(lambda ()
                                 (copyright-update nil t)
                                 (let ((time-stamp-time-zone "UTC"))
                                   (time-stamp))))

  ;; Configure default languages for Google translate
  (setq google-translate-enable-ido-completion t
        google-translate-default-source-language "auto"
        google-translate-default-target-language "en")

  ;; Configure C/C++ modes
  (setq c-default-style '((java  . "java")
                          (awk   . "awk")
                          (other . "k&r")))
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (progn
      (sp-local-pair 'c-mode "'" nil :actions nil)
      (sp-local-pair 'c++-mode "'" nil :actions nil)))
  (add-hook 'c-mode-common-hook 'yagunov//c-mode-common-hook)
  (add-hook 'c++-mode-hook 'yagunov//c++-mode-hook)

  ;; Configure Python environment
  (add-to-list 'auto-mode-alist '("Pipfile\\'" . toml-mode))
  (setq flycheck-python-flake8-executable (concat user-home-directory ".pyenv/shims/flake8")
        flycheck-flake8rc ".flake8")
  (setq py-isort-options (format "--line-width %i" python-fill-column))

  ;; Personal setup
  (yagunov//setup-keybindings)
  (yagunov//setup-faces)

  ;; Open helm below current window.
  (setq helm-split-window-default-side 'below)
  (setq helm-display-function #'helm-default-display-buffer)

  ;; TODO: Open HELM in separate floating frame.
  ;; (setq after-make-frame-functions (remove 'persp-init-new-frame after-make-frame-functions))
  ;; (setq helm-display-function #'helm-display-buffer-in-own-frame)
  )


(defun yagunov//setup-keybindings ()
  "Setup my custom keybindings."

  ;; Unbind 'C-x C-c' because it's too easy to hit accidentally.
  (define-key ctl-x-map (kbd "C-c") nil)

  ;; Adjust default keybindings to my custom ErgoDox layout
  (when (equal system-name "silent-base")
    (global-set-key (kbd "M-:") 'comment-dwim)
    (global-set-key (kbd "C-:") 'eval-expression)
    ;; (define-key evil-insert-state-map (kbd "Ж") (lambda () (interactive) (insert "ж")))
    ;; (define-key evil-insert-state-map (kbd "ж") (lambda () (interactive) (insert "Ж")))
    )

  ;; Prevent from accidental typing of multiple spaces
  (global-set-key (kbd "SPC") 'cycle-spacing)
  (global-set-key (kbd "M-SPC") '(lambda () (interactive) (insert " ")))

  ;; Search kill-ring
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; Manually call completion menu (use C-/ to call helm when in menu)
  (define-key evil-insert-state-map (kbd "TAB") 'company-indent-or-complete-common)

  ;; Disable escape key sequence (default: "fd").
  (setq-default evil-escape-key-sequence nil)

  ;; Enable default evil-lion keybindings:
  (evil-global-set-key 'normal (kbd "g l") 'evil-lion-left)
  (evil-global-set-key 'visual (kbd "g l") 'evil-lion-left)
  (evil-global-set-key 'normal (kbd "g L") 'evil-lion-right)
  (evil-global-set-key 'visual (kbd "g L") 'evil-lion-right)

  ;; Add some new and readjust some existing Spacemacs bindings
  (spacemacs/set-leader-keys
    "fw" 'yagunov/writer-buffer-or-region
    "fF" 'yagunov/switch-window-then-find-file
    "w=" 'yagunov/balance-windows
    "w4" 'yagunov/window-split-quadruple-columns
    "wg" 'spacemacs/window-split-grid
    "ww" 'switch-window
    "we" 'yagunov/exchange-buffer
    "wE" 'switch-window-then-swap-buffer
    "pf" 'helm-projectile-find-file-dwim
    "pF" 'projectile-find-file-dwim-other-window
    "np" 'yagunov/narrow-to-paragraph
    "ot" 'yagunov/google-translate-dwim
    ":"  'ielm))

(defun yagunov//setup-faces ()
  "Setup my custom fonts and colors."
  (custom-set-faces
   '(diff-hl-change ((t (:background "#15568E" :foreground "#15568E"))))
   '(diff-hl-delete ((t (:background "#8E1B15" :foreground "#8E1B15"))))
   '(diff-hl-insert ((t (:background "#4C8E15" :foreground "#4C8E15"))))
   '(switch-window-label ((t (:inherit font-lock-builtin-face :height 15.0))))))

(defadvice recenter (after yagunov/recenter-pulse-line activate)
  "Briefly highlight new centered position."
  (yagunov/pulse-current-line))

(defun yagunov//c-mode-common-hook ()
  (setq c-basic-offset 4)
  ;; Long function arguments indentation like in python-mode.
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  ;; Do not indent lines inside 'extern "C"' constructs.
  (c-set-offset 'inextern-lang 0))

(defun yagunov//c++-mode-hook ()
  (c-set-offset 'inline-open '0)
  (setq comment-start "/* ")
  (setq comment-end " */"))

;; QUICKFIX:
(defun spacemacs/smartparens-pair-newline (id action context)
  (save-excursion
    (indent-according-to-mode)))

;; NOTE: Redefine quick run for rust to enable optimization
(defun spacemacs/rust-quick-run ()
  "Quickly run a Rust file using rustc.
Meant for a quick-prototype flow only - use `spacemacs/open-junk-file' to
open a junk Rust file, type in some code and quickly run it.
If you want to use third-party crates, create a new project using `cargo-process-new' and run
using `cargo-process-run'."
  (interactive)
  (let ((input-file-name (buffer-file-name))
        (output-file-name (concat temporary-file-directory (make-temp-name "rustbin"))))
    (compile
     (format "rustc --C opt-level=3 -o %s %s && %s"
             (shell-quote-argument output-file-name)
             (shell-quote-argument input-file-name)
             (shell-quote-argument output-file-name)))))

;;; init-post-layers.el ends here
