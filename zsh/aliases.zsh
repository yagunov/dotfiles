# File listing:
alias l='ls -sh'
alias lla='ls -lha'
alias all=lla

# Directory tree listing:
alias t="tree -L 2 -I '*~'"
alias td="tree -L 2 -I '*~' -d"
alias tt="tree -I '*~'"
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
alias scp='nocorrect scp -r'
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
alias g=ag
alias gg=grep
alias mtail=multitail
alias f=find-by-name
alias fe=find-by-extention
alias ff=find-file
alias fd=find-directory

# Web:
alias dl="wget -c"
alias wcat='wget -q -O -'
alias dl-dir='wget -rcnpH'
alias youtube-dl="youtube-dl -t"
alias yt="youtube-dl -t"
alias serv-dir="python -m SimpleHTTPServer"

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
    alias m='dmesg -Hx'
    alias cal='ncal -M'
    [ -e $HOME/.pyenv/versions/mycli ] && alias mycli="$HOME/.pyenv/versions/mycli/bin/mycli"
    [ -e $HOME/.pyenv/versions/pgcli ] && alias pgcli="$HOME/.pyenv/versions/pgcli/bin/pgcli"
fi
