ZSH=$HOME/.oh-my-zsh
ZSH_CFG=$(dirname `readlink $HOME/.zshrc`)


# Bootstrap dependencies:
if ! [ -d $ZSH ]; then
    mkdir -p $ZSH
    if whence git &> /dev/null; then
        git clone https://github.com/robbyrussell/oh-my-zsh.git $ZSH
        git clone https://github.com/seebi/dircolors-solarized.git $ZSH/custom/dircolors-solarized
        git clone https://github.com/chriskempson/base16-shell $ZSH/custom/base16-shell
    else
        echo "Warning: git is not available, running zsh without plugins."
    fi
fi

# Activate oh-my-zsh:
if [ -e $ZSH/oh-my-zsh.sh ]; then
    plugins=(git git-flow-avh node python pip pyenv django rvm gem rails vagrant tmux
             go colored-man web-search z)
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

    # Select random color scheme for current terminal
    function rnd_color () {
        themes=($ZSH/custom/base16-shell/base16*$1*.sh)
        N=${#themes[@]}
        ((N=(RANDOM%N)+1))
        COLOR_THEME=${themes[$N]}
        source "$COLOR_THEME"
    }

    # Set colors:
    [ $TERM != "screen" -a -z "$TMUX" -a -z "$SSH_CONNECTION" ] && rnd_color dark
    # eval `dircolors $ZSH/custom/dircolors-solarized/dircolors.ansi-universal`
fi


# Set default editor:
if [ $+SSH_CONNECTION -eq 0 ]; then
    EDITOR="emacsclient --alternate-editor=$ed"
    ALTEDITOR="$EDITOR --no-wait"
else
    EDITOR=vim
    ALTEDITOR=vim
fi


source $ZSH_CFG/bindings.zsh
source $ZSH_CFG/completion.zsh
source $ZSH_CFG/functions.zsh
source $ZSH_CFG/aliases.zsh
