;;; packages.el --- workgroups2 Layer packages File for Spacemacs
;;
;; Copyright (c) 2015, 2016 Andrey Yagunov
;;
;; Author: Andrey Yagunov <yagunov86@gmail.com>
;; URL: TODO: Add link to my github here.
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq workgroups2-packages '(workgroups2))

(defun workgroups2/init-workgroups2 ()
  (use-package workgroups2
    :diminish workgroups-mode
    :config
    (progn
      (defadvice wg-add-buffer-to-buf-list (around helm-performance activate compile)
        "Fix helm performance issue when it's used with workgroups2."
        (when (and buffer
                   (null (string-match "\\*temp.*" (buffer-name buffer))))
          ad-do-it))

      (defun spacemacs//workgroups2-ms-documentation ()
        "Return the docstring for the workspaces micro-state."
        (wg-workgroup-list-display))

      (spaceline-define-segment workspace-number
        "The current workspace name or number. Requires `workgroups-mode' to be
enabled."
        (let* ((name (wg-workgroup-name (wg-current-workgroup)))
               (idx (cl-position (wg-current-workgroup) (wg-workgroup-list-or-error)))
               (str name;; (int-to-string idx)
                    ))
          (if spaceline-workspace-numbers-unicode
              (spaceline--unicode-number str)
            str))
        :when (and active (bound-and-true-p workgroups-mode)))

      ;; Disable default Spacemacs layout management solution
      (if (featurep 'persp-mode)
          (persp-mode -1))

      (spacemacs|define-micro-state workspaces
        :doc (spacemacs//workgroups2-ms-documentation)
        :use-minibuffer t
        :evil-leader "l"
        :bindings                       ; TODO: Disable default C-c z mapping

        ("q"        nil :exit t)
        ("<return>" nil :exit t)

        ;; Sessions:
        ("s"       wg-save-session                   :exit t)
        ("C-s"     wg-save-session                   :exit t)
        ("C-w"     wg-save-session-as                :exit t)
        ("C-f"     wg-open-session                   :exit t)
        ("!"       wg-reset)

        ;; Workgroups:
        ("c"       wg-create-workgroup               :exit t)
        ("C"       wg-clone-workgroup                :exit t)
        ("r"       wg-rename-workgroup               :exit t)
        ("R"       wg-revert-workgroup               :exit t)
        ("C-r"     wg-revert-all-workgroups          :exit t)
        ("C-R"     wg-revert-all-workgroups          :exit t)
        ("k"       wg-kill-workgroup                 :exit t)
        ("K"       wg-delete-other-workgroups        :exit t)
        ("C-k"     wg-kill-workgroup-and-buffers     :exit t)

        ;; WConfig:
        ("M-w"     wg-kill-ring-save-working-wconfig :exit t)
        ("C-y"     wg-yank-wconfig                   :exit t)
        ("y"       wg-yank-wconfig                   :exit t)
        ("u"       wg-undo-wconfig-change)
        ("<left>"  wg-undo-wconfig-change)
        ("<right>" wg-redo-wconfig-change)
        ("U"       wg-redo-wconfig-change)

        ;; Workgroup switching:
        ("n"       wg-switch-to-workgroup-right)
        ("."       wg-switch-to-workgroup-right)
        ("p"       wg-switch-to-workgroup-left)
        (","       wg-switch-to-workgroup-left)
        ("<tab>"   wg-switch-to-previous-workgroup)
        ("l"       wg-switch-to-workgroup            :exit t)
        ("0"       wg-switch-to-workgroup-at-index-0)
        ("1"       wg-switch-to-workgroup-at-index-1)
        ("2"       wg-switch-to-workgroup-at-index-2)
        ("3"       wg-switch-to-workgroup-at-index-3)
        ("4"       wg-switch-to-workgroup-at-index-4)
        ("5"       wg-switch-to-workgroup-at-index-5)
        ("6"       wg-switch-to-workgroup-at-index-6)
        ("7"       wg-switch-to-workgroup-at-index-7)
        ("8"       wg-switch-to-workgroup-at-index-8)
        ("9"       wg-switch-to-workgroup-at-index-9)

        ;; Workgroup movements:
        ("<"       wg-offset-workgroup-left)
        (">"       wg-offset-workgroup-right)

        ("b"       wg-switch-to-buffer               :exit t))

      (setq wg-mode-line-display-on nil
            wg-session-file "~/.emacs.d/.cache/workgroups")

      (flet ((wg-change-modeline nil))
        (workgroups-mode 1)))))
