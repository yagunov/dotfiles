;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; Generic layers:
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-complete-with-key-sequence "kj"
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets"
                      auto-completion-enable-snippets-in-popup t)
     (gtags :variables
            ;; NOTE: ggtags-mode breaks evil-iedit (SPC s e) when active
            gtags-enable-by-default nil)
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
     speed-reading
     theming
     typography
     chrome
     helm

     ;; Input languages:
     (chinese :variables
              chinese-enable-fcitx (not (eq system-type 'darwin))
              chinese-enable-youdao-dict t)

     ;; Editing:
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     evil-cleverparens

     ;; Window and buffer management:
     ibuffer
     ;; Text processing:
     markdown
     org

     ;; Code navigation:
     semantic
     gtags

     ;; Languages:
     emacs-lisp
     (c-c++ :variables c-c++-enable-clang-support t)
     csharp
     fsharp
     (python :variables python-test-runner '(nose pytest))
     ipython-notebook
     ruby
     perl5
     go
     rust
     haskell
     erlang
     elm
     javascript
     ess
     vimscript
     php
     html
     csv
     sql

     ;; Administration:
     ansible
     docker
     vagrant

     ;; My personal layers:
     yagunov-base
     ;; workgroups2
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     rainbow-mode
     helm-flyspell
     strace-mode
     ;; Additional themes:
     gruvbox-theme
     doom-themes
     darktooth-theme
     dracula-theme
     railscasts-reloaded-theme
     flatui-theme
     color-theme-sanityinc-tomorrow
     nubox
     sublime-themes
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; Other possible themes: doom-one, flatland, sanityinc-tomorrow-night, brin,
   ;; busybee, monokai, solarized-dark
   dotspacemacs-themes
   `,(cond ((equal system-name "luminous")
            '(spacemacs-light dracula railscasts-reloaded sanityinc-tomorrow-bight darktooth))
           (t '(gruvbox doom-molokai dracula railscasts-reloaded darktooth)))
   ;; Custom theme modifications
   theming-modifications
   '((dracula
      (font-lock-type-face :foreground "grey")
      (font-lock-comment-face :foreground "#8c9ade")
      (font-lock-doc-face :foreground "#8c9ade"))
     (gruvbox
      (link :foreground "#0dbcde"))
     (kaolin
      (font-lock-comment-face :foreground "light slate gray"))
     (doom-molokai
      (font-lock-comment-face :foreground "#7e7e6e" :underline t)
      (evil-search-highlight-persist-highlight-face :foreground "black" :background "gray")))
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font+. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font (cond ((equal system-name "luminous")
                                    '("Consolas" :size 13 :weight normal :width normal :powerline-scale 1.0))
                                   (t
                                    '("Consolas" :size 13 :weight normal :width normal :powerline-scale 1.0)))
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands.
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 10
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize t
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'left
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p'
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose t
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
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
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start.
   dotspacemacs-auto-resume-layouts t
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  ;; Put emacs customization and auto-saved variables into separate file
  (setq custom-file "~/.emacs.d/.cache/custom.el")
  (load-file custom-file)

  ;; Default input method name looks
  (spacemacs|use-package-add-hook chinese-pyim
    :pre-init (setq-default pyim-title "pinyin"))

  (spacemacs|use-package-add-hook helm-company
    :post-config
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "C-s") 'helm-company)))

  (spacemacs|use-package-add-hook google-translate
    :post-config
    (progn
      (setq google-translate-enable-ido-completion t
            google-translate-default-source-language "auto"
            google-translate-default-target-language "en")))

  (spacemacs|use-package-add-hook magit
    :post-config (setq magit-push-always-verify nil ; Use default branch on 'PP'
                       magit-diff-refine-hunk t
                       magit-diff-options '("--ignore-space-change")))

  (spacemacs|use-package-add-hook cc-mode
    :post-config
    (progn
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

      (add-hook 'c-mode-common-hook 'yagunov//c-mode-common-hook)
      (add-hook 'c++-mode-hook 'yagunov//c++-mode-hook)

      (setq c-default-style '((java  . "java")
                              (awk   . "awk")
                              (other . "k&r"))))))

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
  ;; (set-language-environment "Russian")
  (set-default-coding-systems 'utf-8)
  (set-coding-system-priority 'utf-8 'koi8-r 'cp1251 'cp866)
  ;; Change font for Chinese:
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Hiragino Sans GB" :size 20)))
  ;; Add personal keys:
  (spacemacs/set-leader-keys
    "ss" 'spacemacs/helm-swoop-region-or-symbol
    "sS" 'helm-swoop
    "sp" 'spacemacs/helm-project-smart-do-search-region-or-symbol
    "sP" 'spacemacs/helm-project-smart-do-search
    "w1" 'yagunov/delete-other-windows-vertically-or-all
    "we" 'yagunov/balance-windows-quick-fix
    "w=" 'yagunov/balance-windows-quick-fix
    "ws" 'split-window-below-and-focus
    "wS" 'split-window-below
    "wv" 'split-window-right-and-focus
    "wV" 'split-window-right
    "w <SPC>" 'shrink-window-if-larger-than-buffer
    "ww" 'switch-window
    "wD" 'switch-window-then-delete
    "wm" 'yagunov/swap-buffer
    "wM" 'switch-window-then-maximize
    "bD" 'yagunov/kill-buffer
    "="  'yagunov/dwim-diff
    "fw" 'yagunov/writer-buffer-or-region
    "ot" 'yagunov/google-translate
    "jj" 'avy-goto-char-timer
    ":"  'ielm)
  (evil-global-set-key 'normal "z=" 'flyspell-correct-previous-word-generic)
  (evil-global-set-key 'normal "gl" 'evil-lion-left)
  (evil-global-set-key 'normal "gL" 'evil-lion-right)
  (evil-global-set-key 'visual "gl" 'evil-lion-left)
  (evil-global-set-key 'visual "gL" 'evil-lion-right)
  (global-set-key (kbd "C-M-\\") 'spacemacs/indent-region-or-buffer)
  (global-set-key (kbd "C-h") 'evil-delete-backward-char)
  (global-set-key (kbd "M-h") 'evil-delete-backward-word)
  (global-set-key (kbd "M-:") 'comment-dwim)
  (global-set-key (kbd "M-;") 'eval-expression)
  (global-set-key (kbd "C-:") 'eval-expression)

  (global-set-key (kbd "M-n") 'ahs-forward)
  (global-set-key (kbd "M-p") 'ahs-backward)

  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.07)

  ;; (add-hook 'focus-in-hook (lambda () (linum-relative-mode t)))
  ;; (add-hook 'focus-out-hook (lambda () (linum-relative-mode -1)))

  ;; ;; Fix unimpaired keybindings:
  ;; (yagunov//remap-unimpaired evil-normal-state-map)
  ;; (yagunov//remap-unimpaired evil-motion-state-map)
  ;; (yagunov//remap-unimpaired evil-visual-state-map)
  ;; (define-key evil-motion-state-map "[" 'evil-backward-sentence-begin)
  ;; (define-key evil-motion-state-map "]" 'evil-forward-sentence-begin)

  ;; Apply macro bind to 'q' to all line in region
  (define-key evil-visual-state-map "." ":norm @q")

  ;; Move-selected text
  (define-key evil-visual-state-map (kbd "C-j") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "C-k") (concat ":m '<-2" (kbd "RET") "gv=gv"))
  ;; (define-key evil-visual-state-map (kbd "C-j") 'move-text-region-down)
  ;; (define-key evil-visual-state-map (kbd "C-k") 'move-text-region-up)

  (setq-default clang-format-executable "clang-format-3.7")

  (setq org-directory "~/Documents/notes")
  (setq powerline-default-separator 'bar
        diff-hl-side 'left)
  (set-fringe-mode '(10 . 8))
  (setq-default helm-make-build-dir "build_debug"
                helm-make-command "make -j12 %s")
  ;; Activate helm-swoop from i-search
  (require 'helm-swoop)

  (when (equal system-name "silent-base")
    (setq helm-display-function 'helm-default-display-buffer))

  (add-to-list 'auto-mode-alist '("\\.eye\\'" . ruby-mode))
  (spacemacs|disable-company emacs-lisp-mode) ; it's too slow

  (setq prettify-symbols-unprettify-at-point t)
  (global-prettify-symbols-mode)

  ;; ;; Apply doom-molokai hooks
  ;; (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  ;; (require 'doom-neotree)

  ;; Increase letter size for ace-window
  (custom-set-faces
   '(aw-leading-char-face ((t (:foreground "red" :weight bold :height 5.0 :width normal)))))

  ;; Use this colors in fringe instead of themed ones to display changes
  (custom-set-faces
   '(diff-hl-insert  ((t (:foreground "darkgreen"  :background "darkgreen"))))
   '(diff-hl-change  ((t (:foreground "MediumBlue" :background "MediumBlue"))))
   '(diff-hl-delete  ((t (:foreground "darkred"    :background "darkred"))))
   '(diff-hl-unknown ((t (:foreground "purple3"    :background "purple3"))))))
