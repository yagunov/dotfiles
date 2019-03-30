# User's local binaries:
export PATH=$PATH:$HOME/.dotfiles/bin:$HOME/.local/bin

# Machine specific environment:
[ -x $HOME/.zshenv.local ] && source $HOME/.zshenv.local

# Node.js:
[[ -d $HOME/.npm-packages/bin ]] && export PATH=$HOME/.npm-packages/bin:$PATH

# Rust:
[[ -e $HOME/.cargo/env ]] && source $HOME/.cargo/env

# Go:
[[ -s "/home/silver/.gvm/scripts/gvm" ]] && source "/home/silver/.gvm/scripts/gvm"
