export PATH=$PATH:$HOME/.dotfiles/bin:$HOME/.local/bin

# OS X specific settings:
if [[ `uname` == "Darwin" ]]; then
    export HOMEBREW_GITHUB_API_TOKEN="dcd2e50dea706d5d41e9b890b18c08226c173c1f"
fi

if [ -d $HOME/.npm-packages/bin ]; then
    export PATH=$HOME/.npm-packages/bin:$PATH
fi

if [ -e $HOME/.cargo/env ]; then
    source $HOME/.cargo/env
fi
