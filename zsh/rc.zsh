if [[ `uname` == "Darwin" ]]; then
    READLINK=greadlink
    DIRCOLORS=gdircolors
else
    READLINK=readlink
    DIRCOLORS=dircolors
fi


ZSH=$HOME/.oh-my-zsh
ZSH_CFG=$(dirname `${READLINK} -f $HOME/.zshrc`)
BASE16_SHELL=$ZSH/custom/base16-shell


# Bootstrap dependencies:
if ! [ -d $ZSH ]; then
    mkdir -p $ZSH
    if whence git &> /dev/null; then
        git clone https://github.com/robbyrussell/oh-my-zsh.git $ZSH
        git clone https://github.com/seebi/dircolors-solarized.git $ZSH/custom/dircolors-solarized
        git clone https://github.com/chriskempson/base16-shell $BASE16_SHELL
    else
        echo "Warning: git is not available, running zsh without plugins."
    fi
fi

# Activate oh-my-zsh:
if [ -e $ZSH/oh-my-zsh.sh ]; then
    plugins=(git git-flow-avh node python pip pyenv django rvm gem rails vagrant tmux
             go colored-man web-search z rust cargo)
    if [[ `uname` == "Darwin" ]]; then
       plugins+=(brew)
    fi
    ZSH_THEME="ys"       # fishy
    source $ZSH/oh-my-zsh.sh

    # load extract plunig on first use
    function x () {
        source $ZSH/plugins/extract/extract.plugin.zsh
        extract $@
    }

    # base16-shell themes:
    if [ -n "$PS1" -a -s $BASE16_SHELL/profile_helper.sh ]; then
        eval "$($BASE16_SHELL/profile_helper.sh | grep -v '^\.')"
        # Select random color scheme for current terminal only
        function base16_random () {
            themes=($BASE16_SHELL/scripts/base16*.sh)
            N=${#themes[@]}
            ((N=(RANDOM%N)+1))
            BASE16_THEME=`basename "${themes[$N]}" | sed 's/^base16-\(.*\)\.sh$/\1/'`
            source "${themes[$N]}"
        }
        if [ $TERM != "screen" -a -z "$TMUX" -a -z "$SSH_CONNECTION" ]; then
            [ -s ~/.base16_theme ] && source ~/.base16_theme || _base16 "$BASE16_SHELL/scripts/base16-materia.sh" materia
        fi
    fi

    # Set file/directory colors:
    eval `${DIRCOLORS} $ZSH/custom/dircolors-solarized/dircolors.ansi-universal`
fi


# Set default editor:
if [ $+SSH_CONNECTION -eq 0 ]; then
    export EDITOR="emacsclient --alternate-editor=vim"
    export ALTEDITOR="$EDITOR --no-wait"
else
    export EDITOR=vim
    export ALTEDITOR=vim
fi


source $ZSH_CFG/bindings.zsh
source $ZSH_CFG/completion.zsh
source $ZSH_CFG/functions.zsh
source $ZSH_CFG/aliases.zsh
