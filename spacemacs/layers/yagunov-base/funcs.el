;;; funcs.el --- My personal Layer functions File for Spacemacs
;;
;; Copyright (c) 2015, 2016 Andrey Yagunov
;;
;; Author: Andrey Yagunov <yagunov86@gmail.com>
;; URL: <TODO>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'thingatpt)
(require 'newcomment)
(require 'subword)
(require 'linum)
(require 'ediff)

;; from http://whattheemacsd.com/key-bindings.el-01.html
(defun yagunov/goto-line ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (let ((activated linum-mode))
    (unwind-protect
        (progn
          (unless activated
            (linum-mode 1))
          (goto-line (read-number "Goto line: ")))
      (unless activated
        (linum-mode -1)))))


(defun yagunov/dwim-diff ()
  "Do what I mean diff."
  (interactive)
  (cond ((and mark-active (overlay-buffer mouse-secondary-overlay))
         (let ((secondary-beginning (overlay-start mouse-secondary-overlay))
               (secondary-end (overlay-end mouse-secondary-overlay)))
           (delete-overlay mouse-secondary-overlay)
           (ediff-regions-internal
            (get-buffer (current-buffer)) secondary-beginning secondary-end
            (get-buffer (current-buffer)) (region-beginning) (region-end)
            nil 'ediff-regions-wordwise 'word-mode nil)))
        ((buffer-modified-p) (diff-buffer-with-file))
        (t (vc-diff))))


(defun yagunov/beginning-of-line ()
  "Smarter `beginning-of-line' alternative."
  (interactive)
  (if (eq last-command this-command)
      (beginning-of-line)
    (let ((prev-point (point)))
      (when (ignore-errors
              (unless (comment-search-backward (line-beginning-position) t)
                (beginning-of-line-text)))
        (beginning-of-line))
      (when (= prev-point (point))
        (beginning-of-line)))))

(defun yagunov/end-of-line ()
  "Smarter `end-of-line' alternative."
  (interactive)
  (if (eq last-command this-command)
      (end-of-line)
    (let ((prev-point (point)))
      (when (comment-search-forward (line-end-position) t)
        ;; TODO: Add support for `comment-start' and `comment-end'.
        (goto-char (match-beginning 0))
        (if (looking-back "^\s+")
            (end-of-line)
          (skip-syntax-backward " " (line-beginning-position))))
      (when (= prev-point (point))
        (end-of-line)))))


(defun yagunov/mark-whole-word ()
  "Mark whole word."
  (interactive)
  (when (not (looking-at "\\<"))
    (subword-backward))
  (set-mark (point))
  (subword-forward))

(defun yagunov/set-mark-command (arg)
  "Smart implementation of `set-mark-command'.
Run `set-mark-command' on just one call without prefix argument.
If prefix argument is given then deactivate secondary selection,
if mark is active and point inside of region then convert region
to secondary selection, if called twice run `yagunov/mark-whole-word'."
  (interactive "P")
  (cond (arg
         (delete-overlay mouse-secondary-overlay))
        ((and (eq last-command this-command)
              mark-active
              (= (region-beginning)
                 (region-end)))
         (yagunov/mark-whole-word))
        ((and mark-active
              (>= (point) (region-beginning))
              (<= (point) (region-end)))
         (primary-to-secondary (region-beginning)
                               (region-end))
         (setq deactivate-mark t))
        (t
         (set-mark-command nil))))

(defun yagunov/smart-transpose ()
  "Transpose region and secondary selection in they active or characters otherwise."
  (interactive)
  (if (and mark-active (overlay-buffer mouse-secondary-overlay))
      (call-interactively 'anchored-transpose)
    (call-interactively 'transpose-chars)))


(defun yagunov//english-text-p ()
  (let ((beg (point)))
    (save-excursion
      (save-restriction
        (if (region-active-p)
            (narrow-to-region (region-beginning) (region-end))
          (backward-word)
          (narrow-to-region beg (point)))
        (beginning-of-buffer)
        (looking-at ".*[A-Za-z]+.*")))))

(defun yagunov/google-translate ()
  (interactive)
  (if (yagunov//english-text-p)
      (let ((google-translate-default-source-language "en")
            (google-translate-default-target-language "ru"))
        (call-interactively 'google-translate-at-point))
    (call-interactively 'google-translate-at-point)))


(defun yagunov/other-window-back (count &optional all-frames)
  "Same as `other-window' with negative prfix argument."
  (interactive "p")
  (other-window (- count) all-frames))

(defun yagunov/delete-other-windows-vertically-or-all ()
  "Delete all other windows in current column or if there is non delete all other windows."
  (interactive)
  (or (delete-other-windows-vertically)
      (delete-other-windows)))

;; TODO: Find what causes this bug.
(defun yagunov/balance-windows-quick-fix ()
  (interactive)
  (destructuring-bind (master-win popup-win win-map)
      (popwin:create-popup-window window-min-height :bottom nil)
    (delete-window popup-win))
  (balance-windows))

(defun yagunov//remap-unimpaired (map)
  (let (open-map close-map)
    (map-keymap
     #'(lambda (event binding)
         (cond ((eq event ?[) (setq open-map (copy-keymap binding)))
               ((eq event ?]) (setq close-map (copy-keymap binding)))))
     map)
    (define-key map (kbd "(") open-map)
    (define-key map (kbd ")") close-map)
    (define-key map (kbd "[") nil)
    (define-key map (kbd "]") nil)))

(defun yagunov/writer-buffer-or-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'write-region)
    (call-interactively 'write-file)))
