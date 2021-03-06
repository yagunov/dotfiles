# File and directory listing:
if (( $+commands[exa] )); then
    alias l='exa --group-directories-first'
    alias ls='exa --group-directories-first'
    alias ll='l --time-style=long-iso --git -lh'
    alias la='ll -a'
    alias lla='ll -a'
    alias all=lla
    alias t="ll -T -L 2 -I '*~'"
    alias tt="ll -T -I '*~'"
else
    alias l='ls -sh'
    alias lla='ls -lha'
    alias all=lla
    alias t="tree -L 2 -I '*~'"
    alias tt="tree -I '*~'"
fi
alias td="tree -L 2 -I '*~' -d"
alias ttd="tree -I '*~' -d"

if (( $+commands[bat] )); then
    alias cat=bat
fi

# Disk space:
alias df='df -h'
alias du='du -h'
alias lsblk='lsblk -T -o NAME,TYPE,SIZE,SCHED,MOUNTPOINT,FSTYPE,UUID'

# Working with directories:
alias md='mkdir -p'
alias rd=rmdir

# Working with files:
autoload zmv
alias mmv='noglob zmv -W'       # multiple rename
alias rsync='nocorrect rsync --progress --partial --stats --compress --recursive'
alias mv='nocorrect mv -v'
alias rm='rm -v'
alias cp='nocorrect cp -Rv'
alias scp='nocorrect scp -r3'
alias ssh='nocorrect ssh'
alias touch='nocorrect touch'
alias ln='nocorrect ln'
alias pc='nocorrect rsync -rP'  # copy with progress bar
alias fm='vifm .'

# Administration:
alias sudo='nocorrect sudo'

# Time and date:
alias today='date +"%Y-%m-%d"'
alias now='date +"%Y-%m-%dT%H-%M-%S"'

# Search:
if (( $+commands[rg] )); then
    alias g=rg
elif (( $+commands[ag] )); then
    alias g=ag
else
    alias g="grep -r"
fi
alias gg="grep -r"
alias mtail="multitail -f"

# cargo install fd-find fselect
alias ff='command fd --threads=4 --type=file'
alias fe='command fd --threads=4 --type=file --extension'
alias fd='command fd --threads=4 --type=directory'
alias fsel='noglob fselect'

alias em=edit-match
alias vm=vim-match

# Web:
alias dl="wget -c"
alias wcat='wget -q -O -'
alias dl-dir='wget -rcnpH'
alias youtube-dl="youtube-dl -t"
alias yt="youtube-dl -tic"
alias yta="youtube-dl -ticx"     # audio only
alias serv-dir="python -m SimpleHTTPServer"
alias sr='surfraw'

# Development:
alias make="make -j`getconf _NPROCESSORS_ONLN`"
alias diff="diff -Nau"
alias git='nocorrect git'
alias gst='git status -sb'
alias gl='command git log --decorate --stat'
alias gll='command git log --graph --decorate --pretty=oneline --abbrev-commit --all'
alias p=python-repl
alias fv=hachoir-urwid          # binary format viewer
(( $+commands[fzf] )) && alias man=man-fzf
if (( $+commands[hexyl] )); then
    alias hex="hexyl"
    alias hh="hexyl -n 128"
else
    alias hex="hexdump -C"
    alias hh="hexdump -C -n 128"
fi

# Databases:
alias mysqlshow='nocorrect mysqlshow'

# OS X specific:
if [[ `uname` == "Darwin" ]]; then
    alias tailf="tail -f"
    alias o=open
    alias dmesg="sudo dmesg"
    alias cal='ncal'
fi

# Entertainment:
alias mpv='mpv --audio-channels=stereo --audio-file-auto=fuzzy --no-audio-display'

# Linux specific:
if [[ `uname` == "Linux" ]]; then
    function o () {
        xdg-open $@ &> /dev/null &
        disown
    }
    alias dmesg='command dmesg --time-format=reltime -x'
    alias m='command dmesg -Hxw'
    alias mm='command dmesg -Hx'
    alias cal='ncal -M'
    [ -e $HOME/.pyenv/versions/dbcli ] && alias mycli="$HOME/.pyenv/versions/dbcli/bin/mycli"
    [ -e $HOME/.pyenv/versions/dbcli ] && alias pgcli="$HOME/.pyenv/versions/dbcli/bin/pgcli"
fi
