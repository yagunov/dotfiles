;;; packages.el --- My configuration for Spacemacs (functions)
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

(require 'pulse)


(defun yagunov/writer-buffer-or-region ()
  "Prompt to write buffer or region (when active) content to a file."
  (interactive)
  (if (region-active-p)
      (call-interactively 'write-region)
    (call-interactively 'write-file)))

(defun yagunov/exchange-buffer ()
  "Display an overlay in each window showing a unique key, then
ask user which buffer to swap with the current one"
  (interactive)
  ;; TODO: Exclude current window from selection options.
  (save-selected-window
    (call-interactively 'switch-window-then-swap-buffer)))

(defun yagunov/switch-window-then-find-file ()
  "Helm backed replacement of `switch-window-then-find-file'."
  (interactive)
  (switch-window--then-other-window
   "Find file in window: "
   #'spacemacs/helm-find-files))

(defun yagunov/balance-windows ()
  "Balance window layout based on column width then buffer height."
  (interactive)
  (destructuring-bind (master-win popup-win win-map)
      (popwin:create-popup-window window-min-height :bottom nil)
    (delete-window popup-win))
  (balance-windows))

(defun yagunov/window-split-quadruple-columns (&optional purge)
  "Set the layout to quadruple columns (layout for large monitors).

Uses the function defined in `spacemacs-window-split-delete-function' as a means to
remove windows.

When called with a prefix argument, it uses `delete-other-windows' as a means
to remove windows, regardless of the value in `spacemacs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall spacemacs-window-split-delete-function))

  (if (spacemacs--window-split-splittable-windows)
      (let* ((previous-files (seq-filter #'buffer-file-name
                                         (delq (current-buffer) (buffer-list))))
             (second (split-window-right))
             (third (split-window second nil 'right))
             (fourth (split-window third nil 'right)))
        (set-window-buffer second (or (car previous-files) "*scratch*"))
        (set-window-buffer third (or (cadr previous-files) "*scratch*"))
        (set-window-buffer fourth (or (caddr previous-files) "*scratch*"))
        (balance-windows))
    (message "There are no main windows available to split!")))

(defun yagunov/english-text-p ()
  "Verify that language of word at point or within region is English."
  (let ((beg (point)))
    (save-excursion
      (save-restriction
        (if (region-active-p)
            (narrow-to-region (region-beginning) (region-end))
          (backward-word)
          (narrow-to-region beg (point)))
        (beginning-of-buffer)
        (looking-at ".*[A-Za-z]+.*")))))

(defun yagunov/google-translate-dwim ()
  "Translate thing at point (with automatic English/Russian switch)"
  (interactive)
  (if (yagunov/english-text-p)
      (let ((google-translate-default-source-language "en")
            (google-translate-default-target-language "ru"))
        (call-interactively 'google-translate-at-point))
    (call-interactively 'google-translate-at-point)))

(defun yagunov/eval-print-last-sexp ()
  "Evaluate last SEXP in buffer and insert the result."
  (interactive)
  (end-of-line)
  (eval-print-last-sexp))

(defun yagunov/narrow-to-paragraph (&optional arg)
  "Narrow buffer to current paragraph(s) of text.

A numeric ARG parameter specifies number of forward or backward
pages to include. "
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  ;; NB: 0 (no arg), 1 and -1 should have the same effect.
  (let ((backward (if (< arg -1) arg -1))
        (forward (max 1 (abs arg))))
    (save-excursion
      (widen)
      (forward-paragraph backward)
      (narrow-to-region (point)
                        (progn
                          (forward-paragraph forward)
                          (point))))))

(defun yagunov/pulse-current-line ()
  "Briefly highlight current line with pulse."
  (interactive)
  (let ((pulse-delay 0.05)
        (pulse-iterations 20))
    (pulse-momentary-highlight-one-line (point) 'yagunov/pulse-current-line-face)))


;;; funcs.el ends here
