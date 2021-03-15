if status is-interactive
    # Initial environment setup (first run)
    if not functions -q fisher
        # Set universal variables befor plugins installation
        set -U __done_exclude '^(git (?!push|pull)|tm(ux)?|vim|nvim|e\s|v\s|emacs|mpv|mplayer|vlc|man)'
        set -U Z_CMD "_z"
        set -U ZO_CMD "_zo"

        # Install fisher
        set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
        curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
        fish -c fisher update

        # Disable welcome message
        set -U fish_greeting
    end

    set -gx EDITOR emacsclient --alternate-editor nvim
    set -gx BAT_PAGER less -RFi
    set -gx PAGER bat --theme 1337 --plain

    # FZF configuration
    set -gx FZF_DEFAULT_COMMAND 'command fd --threads 4 --type file'
    set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
    set -gx FZF_DEFAULT_OPTS '--ansi --height 50% --reverse --border --multi --inline-info --no-mouse'

    # Prompt
    starship init fish | source
end

# TODO https://github.com/oh-my-fish/plugin-expand (does not work outside of
#   OMF) or https://gitlab.com/Lotz/fish_globabbr
# TODO https://github.com/oh-my-fish/plugin-gi
# TODO Maybe https://github.com/oh-my-fish/marlin instead of z
# TODO await (command that waits for termination of a process (by pid or name))
# TODO x (extract) and pack
