# User's local binaries:
export PATH=$PATH:$HOME/.dotfiles/bin:$HOME/.local/bin

function .source-file () {
    [ -e "$1" ] && source "$1"
}

# Doom emacs distribution
[ -d $HOME/.emacs/bin ] && export PATH=$HOME/.emacs/bin:$PATH

# Machine specific environment:
.source-file "$HOME/.zshenv.local"

# Node.js:
[ -d $HOME/.npm-packages/bin ] && export PATH=$HOME/.npm-packages/bin:$PATH

# Rust:
.source-file "$HOME/.cargo/env"

# Go:
.source-file "$HOME/.gvm/scripts/gvm"

# OCaml:
.source-file "$HOME/.opam/opam-init/init.zsh"
