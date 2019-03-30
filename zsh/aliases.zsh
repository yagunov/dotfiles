# File listing:

# Directory listing:
if (( $+commands[exa] )); then
    alias l='exa --group-directories-first'
    alias ll='exa --group-directories-first -l'
    alias lla='exa --group-directories-first -al'
    alias all=lla
    alias t="exa --group-directories-first -l -T -L 2 -I '*~'"
    alias tt="exa --group-directories-first -l -T -I '*~'"
else
    alias l='ls -sh'
    alias lla='ls -lha'
    alias all=lla
    alias t="tree -L 2 -I '*~'"
    alias tt="tree -I '*~'"
fi
alias td="tree -L 2 -I '*~' -d"
alias ttd="tree -I '*~' -d"

# Disk space:
alias df='df -h'
alias du='du -h'

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

# Administration:
alias sudo='nocorrect sudo'

# Time and date:
alias today='date +"%Y-%m-%d"'
alias now='date +"%Y-%m-%dT%H-%M-%S"'

# Search:
if (( $+commands[tag] )); then
    alias g=tag
elif (( $+commands[rg] )); then
    alias g=rg
elif (( $+commands[ag] )); then
    alias g=ag
else
    alias g=grep
fi
alias gg=grep
alias mtail="multitail -f"
alias f=find-by-name
alias fe=find-by-extention
alias ff=find-file
alias fd=find-directory

# Web:
alias dl="wget -c"
alias wcat='wget -q -O -'
alias dl-dir='wget -rcnpH'
alias youtube-dl="youtube-dl -t"
alias yt="youtube-dl -tic"
alias yta="youtube-dl -ticx"     # audio only
alias serv-dir="python -m SimpleHTTPServer"
alias sr='surfraw'

# Strong password generation:
alias pwgen="pwgen -C 20 -B -s 1"

# Development:
alias make="make -j`getconf _NPROCESSORS_ONLN`"
alias hex="hexdump -C"
alias hh="hexdump -C -n 128"
alias diff="diff -Nau"
alias git='nocorrect git'
alias gst='git status -sb'
alias p=run-python
alias fv=hachoir-urwid          # binary format viewer

# Databases:
alias mysqlshow='nocorrect mysqlshow'

# OS X specific:
if [[ `uname` == "Darwin" ]]; then
    alias tailf="tail -f"
    alias o=open
    alias dmesg="sudo dmesg"
    alias cal='ncal'
fi

# Linux specific:
if [[ `uname` == "Linux" ]]; then
    function o () {
        xdg-open $@ &> /dev/null &
        disown
    }
    alias dmesg='dmesg -T'
    alias m='dmesg -Hxw'
    alias mm='dmesg -Hx'
    alias cal='ncal -M'
    [ -e $HOME/.pyenv/versions/mycli ] && alias mycli="$HOME/.pyenv/versions/mycli/bin/mycli"
    [ -e $HOME/.pyenv/versions/pgcli ] && alias pgcli="$HOME/.pyenv/versions/pgcli/bin/pgcli"
fi
