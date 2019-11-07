# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

if [ "$0" = "/etc/gdm3/Xsession" -a "$DESKTOP_SESSION" = "i3" ]; then
    export $(gnome-keyring-daemon --start --components=ssh)
fi

export PATH="$HOME/.dotfiles/bin:$HOME/.local/bin:$PATH"

function .source-file () {
    [ -e "$1" ] && source "$1"
}

# Rust:
.source-file "$HOME/.cargo/env"

# Python:
[ -d $HOME/.pyenv/bin ] && export PATH="$HOME/.pyenv/bin:$HOME/.pyenv/shims:$PATH"

# JavaScript:
[ -d $HOME/.npm-packages/bin ] && export PATH="$HOME/.npm-packages/bin:$PATH"

# Go:
.source-file "$HOME/.gvm/scripts/gvm"

# OCaml:
.source-file "$HOME/.opam/opam-init/init.sh"
