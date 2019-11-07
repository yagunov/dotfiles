# Restart this shell
function restart () {
    exec $SHELL $SHELL_ARGS "$@"
}

# Edit file/directory.
function e () {
    if [[ $# == 0 ]]; then
        if [ -t 0 ]; then
            # when run without arguments open current directory
            eval $ALTEDITOR .
        else
            # redirect stdin to temporary file then open it with editor
            tmp=`mktemp /tmp/emacs-stdin-XXX`
            cat > "$tmp"
            eval $ALTEDITOR "$tmp"
        fi
    else
        # NOTE: This is a work around for file names with spaces in it.
        echo -n "$@" | eval xargs -0 $ALTEDITOR
    fi
}

function edit-match () {
    place=`rg --color=always --line-number $@ | fzf`
    if (( $+commands[emacsclient] )); then
        cmd=`echo "$place" | sed -rn 's/^(.*):([[:digit:]]*):.*/emacsclient --alternate-editor=vim --no-wait +\2 \1/p'`
    else
        cmd=`echo "$place" | sed -rn 's/^(.*):([[:digit:]]*):.*/vim \1 +\2/p'`
    fi
    eval "$cmd"
}

function edit-file () {
    local pattern="*$1*"; [[ $# > 0 ]] && shift;
    local file=`find . -iname "$pattern" $@ | fzf -1`
    if (( $+commands[emacsclient] )); then
        emacsclient --alternate-editor=vim --no-wait "$file"
    else
        cmd=`echo "$place" | sed -rn 's/^(.*):([[:digit:]]*):.*/vim \1 +\2/p'`
        vim "$file"
    fi
}

function vim-match () {
    place=`rg --color=always --line-number $@ | fzf`
    eval "$(echo "$place" | sed -rn 's/^(.*):([[:digit:]]*):.*/vim \1 +\2/p')"
}

function h () {
    if [[ $# == 0 ]]; then
        history | tail -10
    else
        history | grep $@
    fi
}

function man-fzf () {
    if [[ $# == 0 ]]; then
        local topic=`command man -k . | fzf | cut -d ' ' -f 1`
        [ -n "$topic" ] && man "$topic"
    else
        man $@
    fi
}

# Watch system log:
function wlog () {
    local log_file="/var/log/syslog$([[ `uname` == "Darwin" ]] && echo .log)"
    local tail_cmd="multitail -f ${1:+-e $1} $log_file"

    if [ -n "$TMUX" ]; then
        tmux new-window -n "log${1:+:$1}" $tail_cmd
        tmux split-window -v -p 10 dmesg -Tw
        tmux select-pane -U
    else
        $tail_cmd
    fi
}

function slog () {
    local log_file="/var/log/syslog$([[ `uname` == "Darwin" ]] && echo .log)"

    if [[ $# == 0 ]]; then
        less "$log_file"
    else
        cat "$log_file" | grep $@
    fi
}

function tm () {
    [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"
    if [ $1 ]; then
        tmux $change -t "$1" 2>/dev/null || {
            case "$1" in
                top)
                    base16_hopscotch
                    ;;
                mid)
                    base16_material-darker
                    ;;
                bot)
                    base16_material-palenight
                    ;;
                scratch)
                    base16_material
                    ;;
            esac
            tmux -2u new-session -d -s "$1" && tmux $change -t "$1"; return
        }
    fi
    session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --exit-0) &&  tmux $change -t "$session" || echo "No sessions found."
}

function gh () {
    local commit=`git log --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |  fzf | cut -d ' ' -f 1`
    if [ -n "$commit" ]; then
        echo -n "$commit" | xsel -i
        echo "$commit"
    fi
}

# Git Checkout from History
function gch () {
    local commit=`git log --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |  fzf | cut -d ' ' -f 1`
    if [ -n "$commit" ]; then
        echo -n "$commit" | xsel -i
        git checkout "$commit"
    fi
}

function gcb () {
    local branch=`git branch --color=always -av | fzf | cut -d ' ' -f 3`
    if [ -n "$branch" ]; then
        echo -n "$branch" | xsel -i
        git checkout "$branch"
    fi
}

# pwgen wrapper
function pwgen () {
    if (( $# == 0 )); then
        # Generate strong password and copy it into clipboard
        local password=`command pwgen -C 20 -B -s 1`
        if [ -n "$password" ]; then
            echo -n "$password" | xsel -i
            echo "$password"
        fi
    else
        command pwgen -s $@
    fi
}

# wait for process start
function wstart () {
    while ! pidof $1 &> /dev/null; do
        sleep 1
    done
}

# wait for process termination
function wterm () {
    while pidof $1 &> /dev/null; do
        sleep 1
    done
}

#
# Search:
#

function tree () {
    if [ -t 1 ]; then
        # stdout is a terminal: use colors
        command tree --dirsfirst -C $@
    else
        # stdout is a pipe or a file: don't use colors
        command tree --dirsfirst -n $@
    fi
}

function du-top () {
    du -h --max-depth=1 "$@" | sort -hr
}

#
# CMake wrappers:
#
function cmake-debug() {
    mkdir -p build_debug && cd build_debug && cmake -DCMAKE_BUILD_TYPE=Debug ..
}

function cmake-release() {
    mkdir -p build_release && cd build_release && cmake -DCMAKE_BUILD_TYPE=Release ..
}

function cmake-clean() {
    if [[ -n $(find . -maxdepth 1 -name 'build_*' -type d 2> /dev/null) ]]; then
        # Remove out-of-source builds
        find . -maxdepth 1 -name 'build_*' -type d | xargs rm -rf
    elif [ -d CMakeFiles ]; then
        # Clean up after in-source build
        find . -name "CMakeFiles" | xargs rm -rf
        find . -name "Testing" | xargs rm -rf
        find . -name "CMakeCache.txt" | xargs rm -f
        find . -name "CTestTestfile.cmake" | xargs rm -f
        find . -name "Makefile" | xargs rm -f
        find . -name "cmake_install.cmake" | xargs rm -f
    fi
}

function pj-build() (
    BUILD_TYPE="${1:-release}"
    PROJ_ROOT="$(git root 2> /dev/null || pwd)"

    # Detect project root
    cd "${PROJ_ROOT}"

    # Detect project type
    if [ -e CMakeLists.txt ]; then
        PROJ_TYPE="CMake"
        cmake-$BUILD_TYPE && make -j`getconf _NPROCESSORS_ONLN`
    elif [ -e Makefile ]; then
        PROJ_TYPE="Make"
        make -j`getconf _NPROCESSORS_ONLN`
    elif [ -e Cargo.toml ]; then
        PROJ_TYPE="Cargo"
        if [[ "$BUILD_TYPE" == "release" ]]; then
            cargo build --release
        else
            cargo build
        fi
    else
        echo "No project found"
        exit 1
    fi

    if [[ $? == 0 ]]; then
        notify-send --icon=process-completed -u normal "$PROJ_TYPE [$BUILD_TYPE]: Build successful" "$PROJ_ROOT"
    else
        notify-send --icon=system-error -u critical "$PROJ_TYPE [$BUILD_TYPE]: Build failed ($?)" "$PROJ_ROOT"
    fi
)


function ds() {
    if [ $# -ne 0 ]; then
        # compare files/directories
        diff -Nau "$1" "$2" | diffstat -C
    elif [ -t 0 ]; then
        # no input
        git diff | diffstat -C
    else
        # input is PIPE
        diffstat -C
    fi
}


# Unix time
function ut() {
    if [[ $# == 0 ]]; then
        # Get current time as Unix timestamp
        date +%s
    else
        # Convert Unix timestamp to human readable date and time
        date -d @$1
    fi
}


# synchronize directories on different machines
function syncdirs () {
    # FIXME: Using --delete can lead to loss of files. Need a backup.
    rsync --progress --stats --compress --rsh=ssh \
          --recursive --times --perms --links --delete $@
}

#
# Python development:
#

function python-repl () {
    # TODO: Does not work properly with pyenv due to shims
    local VERSION=$1
    if [ -n "$VIRTUAL_ENV" ]; then
        # we are inside virtual environment
        if [ -x "$VIRTUAL_ENV"/bin/ptipython ]; then
            "$VIRTUAL_ENV"/bin/ptipython
        elif [ -x "$VIRTUAL_ENV"/bin/ipython$VERSION ]; then
            "$VIRTUAL_ENV"/bin/ipython$VERSION
        elif [ -x "$VIRTUAL_ENV"/bin/python$VERSION ]; then
            "$VIRTUAL_ENV"/bin/python$VERSION
        else
            # fallback to system's version
            (which ipython$VERSION &> /dev/null) && ipython$VERSION || python$VERSION
        fi
    else
        # using system's python
        if which ptipython$VERSION &> /dev/null; then
            ptipython$VERSION
        elif which ipython$VERSION &> /dev/null; then
            ipython$VERSION
        else
            python$VERSION
        fi
    fi
}
