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

  ;; Personal setup
  (yagunov//setup-keybindings)
  (yagunov//setup-faces))


(defun yagunov//setup-keybindings ()
  "Setup my custom keybindings."
  (spacemacs/set-leader-keys
    "fw" 'yagunov/writer-buffer-or-region
    "fF" 'yagunov/switch-window-then-find-file
    "w=" 'yagunov/balance-windows
    "w4" 'yagunov/window-split-quadruple-columns
    "wg" 'spacemacs/window-split-grid
    "ww" 'switch-window
    "we" 'yagunov/exchange-buffer
    "wE" 'switch-window-then-swap-buffer
    "wM" 'ace-maximize-window
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


;;; init-post-layers.el ends here
