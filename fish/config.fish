source ~/.asdf/asdf.fish

# set -U Z_CMD "_z"
# set -U Z9_CMD "_zo"
# set -U __done_exclude '^(git (?!push|pull)|vim|nvim|emacs|mpv|mplayer|vlc)'

if status is-interactive
    set -gx EDITOR 'emacsclient --no-wait --alternate-editor=vim'
    set -gx PAGER 'bat --theme=1337 -p'

    # fzf configuration
    set -gx FZF_DEFAULT_COMMAND 'fd --threads=4 --type f'
    set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
    set -gx FZF_DEFAULT_OPTS '--ansi --height 50% --reverse --border --multi --inline-info --no-mouse'

    # Prompt
    starship init fish | source
end

# abbr -a -U -- - 'cd -'
# abbr -a -U -- dm 'dmesg --time-format=reltime -x -w'
# abbr -a -U -- du 'du -sh'
# abbr -a -U -- e 'emacsclient --no-wait'
# abbr -a -U -- g 'rg -i'
# abbr -a -U -- gb 'git branch -av'
# abbr -a -U -- gc 'git checkout'
# abbr -a -U -- gcl 'git clone'
# abbr -a -U -- gd 'git diff'
# abbr -a -U -- gh 'git head'
# abbr -a -U -- gl 'git log --stat'
# abbr -a -U -- gp 'git pull'
# abbr -a -U -- gst 'git status -sb'
# abbr -a -U -- gwd git\ diff\ --word-diff-regex=\'\\w+\'
# abbr -a -U -- md 'mkdir -p'
# abbr -a -U -- n 'nextd -l'
# abbr -a -U -- now 'date +%Y-%m-%dT%H-%M-%S'
# abbr -a -U -- p 'prevd -l'
# abbr -a -U -- rd 'rm -r'
# abbr -a -U -- restart 'exec fish'
# abbr -a -U -- sc systemctl
# abbr -a -U -- scu 'systemctl --user'
# abbr -a -U -- sd sudo
# abbr -a -U -- today 'date +%Y-%m-%d'
# abbr -a -U -- ut 'date +%s'


# TODO https://github.com/oh-my-fish/plugin-expand or https://gitlab.com/Lotz/fish_globabbr
# TODO https://github.com/oh-my-fish/plugin-gi
# TODO Maybe https://github.com/oh-my-fish/marlin instead of z
# TODO await (command that waits for termination of a process (by pid or name))
# TODO x (extract) and pack
