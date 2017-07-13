;;; funcs.el --- My personal Layer key bindings File
;;
;; Copyright (c) 2015, 2016, 2017 Andrey Yagunov
;;
;; Author: Andrey Yagunov <yagunov86@gmail.com>
;; URL: <TODO>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Golf3

(require 'bind-key)

;; Smarter text navigation
(global-set-key (kbd "C-a") 'yagunov/beginning-of-line)
(global-set-key (kbd "C-e") 'yagunov/end-of-line)

;; (bind-key* "C-;" 'split-window-horizontally)
;; (bind-key* "C-'" 'split-window-vertically)
(global-set-key (kbd "C-x 1") 'yagunov/delete-other-windows-vertically-or-all)

;; More visual goto-line
(global-set-key (kbd "M-g g") 'yagunov/goto-line)
(global-set-key (kbd "M-g M-g") 'yagunov/goto-line)

;; Smarter diff for current buffer
(global-set-key (kbd "C-x v =") 'yagunov/dwim-diff)

;; Use helm by default
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)

;; Move text like in Prelude
(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-p") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)
(global-set-key (kbd "C-S-n") 'move-text-down)

;; Prevent from accidental typing of multiple spaces
(global-set-key (kbd "SPC") 'cycle-spacing)
(global-set-key (kbd "M-SPC") '(lambda () (interactive) (insert " ")))

(bind-key* "C-." 'helm-flyspell-correct)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; ;; Fix problems with my custom keyboard layout:
;; (global-set-key [remap toggle-input-method] 'yagunov/toggle-input-method)

;; Faster window navigation
(require 'dired)
(define-key dired-mode-map (kbd "C-o") nil)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-M-o") 'yagunov/other-window-back)

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (spacemacs/set-leader-keys-for-major-mode mode
    "ep" 'eval-print-last-sexp))

(require 'time-stamp)
(require 'copyright)
(add-hook 'before-save-hook
          '(lambda ()
             (copyright-update nil t)
             (let ((time-stamp-time-zone "UTC"))
               (time-stamp))))
