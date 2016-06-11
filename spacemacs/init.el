;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; Generic layers:
     (auto-completion
      :variables
      auto-completion-return-key-behavior 'complete
      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets"
      auto-completion-enable-snippets-in-popup t)
     spell-checking
     ;; syntax-checking
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'eshell)
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-tool 'diff-hl)
     git
     github
     (ranger :variables
             ranger-show-preview t)

     search-engine                      ; add 'SPC a /' for web search
     ;; TODO: Use theming layer

     ;; Editing:
     evil-snipe

     ;; Window and buffer management:
     ibuffer
     ;; Text processing:
     markdown
     org

     ;; Languages:
     semantic
     emacs-lisp
     (c-c++ :variables c-c++-enable-clang-support t)
     csharp
     fsharp
     (python :variables python-test-runner '(nose pytest))
     ruby
     go
     rust
     haskell
     erlang
     ess
     vimscript
     php
     html
     csv
     sql

     ;; Administration:
     ansible
     dockerfile
     vagrant

     ;; My personal layers:
     yagunov-base
     ;; workgroups2
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(rainbow-mode helm-flyspell)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes `(,(cond ((equal system-name "luminous.local")
                                 'material-light)
                                (t 'material))
                         brin
                         busybee
                         monokai
                         solarized-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font+. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font (cond ((equal system-name "luminous.local")
                                    '("Source Code Pro" :size 12 :weight normal :width normal :powerline-scale 1.0))
                                   (t
                                    '("Consolas" :size 11 :weight normal :width normal :powerline-scale 1.0)))
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start.
   dotspacemacs-auto-resume-layouts t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  ;; Personal information:
  (setq user-full-name "Andrey Yagunov"
        user-mail-address "yagunov86@gmail.com")
  ;; Tab width:
  (setq-default tab-width 4)
  ;; Set language environment and coding system
  (set-language-environment "Russian")
  (set-default-coding-systems 'utf-8)
  (set-coding-system-priority 'utf-8 'koi8-r 'cp1251 'cp866)
  ;; Add personal keys:
  (spacemacs/set-leader-keys
    "ss" 'spacemacs/helm-swoop-region-or-symbol
    "sS" 'helm-swoop
    "sp" 'spacemacs/helm-project-smart-do-search-region-or-symbol
    "sP" 'spacemacs/helm-project-smart-do-search
    "w1" 'yagunov/delete-other-windows-vertically-or-all
    "we" 'yagunov/balance-windows-quick-fix
    "w=" 'yagunov/balance-windows-quick-fix
    "w_" 'shrink-window-if-larger-than-buffer
    "="  'yagunov/dwim-diff
    "fw" 'yagunov/writer-buffer-or-region)
  (global-set-key (kbd "C-M-\\") 'spacemacs/indent-region-or-buffer)
  (global-set-key (kbd "C-h") 'evil-delete-backward-char)
  (global-set-key (kbd "M-h") 'evil-delete-backward-word)
  (global-set-key (kbd "M-:") 'comment-dwim)
  (global-set-key (kbd "M-;") 'eval-expression)

  ;; (add-hook 'focus-in-hook (lambda () (linum-relative-mode t)))
  ;; (add-hook 'focus-out-hook (lambda () (linum-relative-mode -1)))

  ;; Fix unimpaired keybindings:
  (yagunov//remap-unimpaired evil-normal-state-map)
  (yagunov//remap-unimpaired evil-motion-state-map)
  (yagunov//remap-unimpaired evil-visual-state-map)
  (define-key evil-motion-state-map "[" 'evil-backward-sentence-begin)
  (define-key evil-motion-state-map "]" 'evil-forward-sentence-begin)

  (setq org-directory "~/Documents/notes")
  (setq powerline-default-separator 'bar
        diff-hl-side 'left)
  (when (equal system-name "luminous")
    (set-fringe-mode '(15 . 10)))
  (setq-default helm-make-build-dir "build_debug"
                helm-make-command "make -j12 %s")
  ;; Activate helm-swoop from i-search
  (require 'helm-swoop))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(evil-disable-insert-state-bindings t)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#37474f" t)
 '(frame-brackground-mode (quote dark))
 '(hl-sexp-background-color "#1c1f26")
 '(linum-format " %5i ")
 '(magit-pull-arguments nil)
 '(package-selected-packages
   (quote
    (hlint-refactor flyspell-correct-helm vimrc-mode dactyl-mode magit-gh-pulls github-clone github-browse-file git-link gist gh logito pcache myrth-theme goto-chg undo-tree diminish flyspell-correct omnisharp csharp-mode flycheck fsharp-mode phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode org-download py-yapf evil-visual-mark-mode ox-reveal web-mode tagedit slim-mode scss-mode sass-mode less-css-mode jade-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data csv-mode sql-indent hide-region dockerfile-mode nlinum column-enforce-mode deferred busybee-theme sublime-themes monokai-theme color-theme-sanityinc-tomorrow rainbow-mode evil-snipe alert log4e gntp parent-mode pkg-info epl request fringe-helper flx iedit ctable pos-tip rust-mode spinner magit-popup go-mode ghc haskell-mode inf-ruby yasnippet pythonic auto-complete packed julia-mode highlight anzu with-editor helm-core async projectile avy hydra f anaphora ::material-theme rake vagrant-tramp vagrant yaml-mode jinja2-mode ansible-doc ansible uuidgen live-py-mode link-hint rustfmt helm-hoogle evil-ediff bracketed-paste xterm-color eshell-z ess-smart-equals ess-R-object-popup ess-R-data-view ess ws-butler persp-mode lorem-ipsum hl-todo help-fns+ evil-magit evil-indent-plus ace-jump-helm-line bind-map smartparens ranger highlight-symbol enh-ruby-mode accelerate orgit toml-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv racer company-racer chruby bundler engine-mode git-gutter-fringe+ git-gutter-fringe git-gutter+ git-gutter restart-emacs powerline evil-org diff-hl macrostep second-sel anchored-transpose workgroups2 airline-themes material-theme eyebrowse stickyfunc-enhance srefactor helm-flx auto-compile shm ibuffer-projectile hindent haskell-snippets go-eldoc erlang company-go company-ghc company-cabal cmm-mode toc-org smeargle shell-pop pyvenv pytest pyenv-mode pip-requirements org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets multi-term mmm-mode markdown-toc markdown-mode magit-gitflow magit hy-mode htmlize helm-pydoc helm-gitignore helm-flyspell helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-commit gh-md eshell-prompt-extras esh-help disaster cython-mode company-statistics company-quickhelp company-c-headers company-anaconda company cmake-mode clang-format auto-yasnippet anaconda-mode ac-ispell window-numbering volatile-highlights vi-tilde-fringe spray spaceline smooth-scrolling rainbow-delimiters popwin popup pcre2el paradox page-break-lines open-junk-file neotree move-text linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-descbinds helm-ag helm google-translate golden-ratio flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu elisp-slime-nav define-word clean-aindent-mode buffer-move auto-highlight-symbol auto-dictionary aggressive-indent adaptive-wrap ace-window ace-link evil-leader evil which-key quelpa package-build use-package bind-key s dash spacemacs-theme)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (c-set-offset
            (quote arglist-intro)
            (quote +))
           (c-set-offset
            (quote arglist-close)
            (quote 0))
           (c-set-offset
            (quote inextern-lang)
            0))
     (time-stamp-time-zone quote UTC))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(wg-current-workgroup-face ((t (:inherit font-lock-constant-face :foreground "orange" :weight bold)))))
