;;; packages.el --- My configuration for Spacemacs
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `yagunov-base-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `yagunov-base/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `yagunov-base/pre-init-PACKAGE' and/or
;;   `yagunov-base/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst yagunov-base-packages
  '(persistent-scratch
    switch-window)
  "The list of Lisp packages required by the yagunov-base layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun yagunov-base/init-persistent-scratch ()
  (use-package persistent-scratch
    :config
    (persistent-scratch-setup-default)))

(defun yagunov-base/init-switch-window ()
  (use-package switch-window
    :init
    (progn
      (setq switch-window-shortcut-style 'qwerty
            switch-window-minibuffer-shortcut nil
            switch-window-qwerty-shortcuts
            '("a" "s" "d" "f" "j" "k" "l" ":" "w" "e" "i" "o" "g" "h" "r" "q" "u" "v" "n"))

      ;; Redefine core ace-window function
      (with-eval-after-load 'ace-window
        (fset 'aw-select-orign (symbol-function 'aw-select))
        (defun aw-select (prompt &optional action)
          "Return a selected other window (with switch-window when possible)."
          (let* ((index (switch-window--prompt prompt))
                 (window (cl-loop for c from 1
                                  for win in (switch-window--list)
                                  until (= c index)
                                  finally return win)))
            (if action
                (funcall action window)
              window)))))))

;;; packages.el ends here
