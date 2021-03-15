# TODO Remove old channels?

nix-channel --add https://nixos.org/channels/nixos-unstable nixos 
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --add https://github.com/nix-community/emacs-overlay/archive/master.tar.gz emacs
nix-channel --update

ln -sf `realpath ./configuration.nix` /etc/nixos/configuration.nix
ln -sf `realpath ./hosts/$1` /etc/nixos/host
