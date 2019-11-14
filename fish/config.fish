source ~/.asdf/asdf.fish

# set -U Z_CMD "_z"
# set -U Z9_CMD "_zo"
# set -U __done_exclude '^(git (?!push|pull)|tm(ux)?|vim|nvim|e\s|v\s|emacs|mpv|mplayer|vlc)'

if status is-interactive
    set -gx EDITOR emacsclient --no-wait --alternate-editor vim
    set -gx PAGER bat --theme 1337 -p

    # fzf configuration
    set -gx FZF_DEFAULT_COMMAND 'command fd --threads 4 --type file'
    set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
    set -gx FZF_DEFAULT_OPTS '--ansi --height 50% --reverse --border --multi --inline-info --no-mouse'

    # Prompt
    starship init fish | source
end

# TODO https://github.com/oh-my-fish/plugin-expand or https://gitlab.com/Lotz/fish_globabbr
# TODO https://github.com/oh-my-fish/plugin-gi
# TODO Maybe https://github.com/oh-my-fish/marlin instead of z
# TODO await (command that waits for termination of a process (by pid or name))
# TODO x (extract) and pack
