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

function h () {
    if [[ $# == 0 ]]; then
        history | tail -10
    else
        history | grep $@
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

#
# Search:
#

# find files/direcories by name
function find-by-name () {
    find . -iname "*$1*"
}

# find by extension
function find-by-extention () {
    ext="$1"; shift; find . -iname "*.$ext" -type f $@
}

# find only files
function find-file () {
    find . -iname "*$1*" -type f
}

# find only directories
function find-directory () {
    find . -iname "*$1*" -type d
}

# find executable
function find-executable () {
    find . -iname "*$1*" -type f -perm /a=x
}


function tree () {
    if [ -t 1 ]; then
        # stdout is a terminal: use colors
        command tree --dirsfirst -C $@
    else
        # stdout is a pipe or a file: don't use colors
        command tree --dirsfirst -n $@
    fi
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


function ds() {
    if [ $# -ne 0 ]; then
        # compare files/directories
        diff -Nau "$1" "$2" | diffstat
    elif [ -t 0 ]; then
        # no input
        git diff | diffstat
    else
        # input is PIPE
        diffstat
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

function run-python() {
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

function p2() (
    pyenv shell 2.7.11
    run-python
)

function p3() (
    pyenv shell 3.5.1
    run-python
)
