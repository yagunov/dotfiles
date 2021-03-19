if status is-interactive; and not set -Uq fish_abbreviations_initialized
    # - to cd to previously visited directory
    abbr -a -U -- - 'cd -'

    # Files: listing
    abbr -a -U -- all lla
    abbr -a -U -- tta 'tt --all'
    abbr -a -U -- ta 't --all'
    abbr -a -U -- du 'dua -f binary'
    abbr -a -U -- fm 'vifm .'
    abbr -a -U -- df 'df -h --total'

    # Files: view, copy, modify
    abbr -a -U -- cp 'cp -ivr'
    abbr -a -U -- xcp 'xcp -r'
    abbr -a -U -- scp 'scp -r3'
    abbr -a -U -- mv 'mv -iv'
    abbr -a -U -- rd 'rm -rv'
    abbr -a -U -- rf 'rm -vfr'
    abbr -a -U -- md 'mkdir -p'
    abbr -a -U -- cm chmod
    abbr -a -U -- c cat
    abbr -a -U -- hd head
    abbr -a -U -- tl tail
    abbr -a -U -- tf 'tail -f'
    abbr -a -U -- dd4k 'dd bs=4k if='
    abbr -a -U -- dd4m 'dd bs=4M if='

    # Files: search
    abbr -a -U -- fai 'fa -IH'  # -IH means --no-ignore --hidden
    abbr -a -U -- fdi 'fd -IH'
    abbr -a -U -- fei 'fe -IH'
    abbr -a -U -- ffi 'ff -IH'
    abbr -a -U -- fli 'fl -IH'
    abbr -a -U -- fxi 'fx -IH'
    abbr -a -U -- fa1 'fa --max-depth=1'
    abbr -a -U -- fd1 'fd --max-depth=1'
    abbr -a -U -- fe1 'fe --max-depth=1'
    abbr -a -U -- ff1 'ff --max-depth=1'
    abbr -a -U -- fl1 'fl --max-depth=1'
    abbr -a -U -- fx1 'fx --max-depth=1'
    abbr -a -U -- fa1i 'fa -IH --max-depth=1'
    abbr -a -U -- fd1i 'fd -IH --max-depth=1'
    abbr -a -U -- fe1i 'fe -IH --max-depth=1'
    abbr -a -U -- ff1i 'ff -IH --max-depth=1'
    abbr -a -U -- fl1i 'fl -IH --max-depth=1'
    abbr -a -U -- fx1i 'fx -IH --max-depth=1'
    abbr -a -U -- g 'rg -i'
    abbr -a -U -- gi 'rg -iv'
    abbr -a -U -- gg 'rg -i --no-ignore --hidden'

    # Development
    abbr -a -U -- cg cargo
    abbr -a -U -- cgs cargo search
    abbr -a -U -- d diff
    abbr -a -U -- mk 'make -j12'
    abbr -a -U -- nj ninja
    abbr -a -U -- vim nvim
    abbr -a -U -- vd 'nvim -d'
    abbr -a -U -- sv 'sudo nvim'
    abbr -a -U -- hf hyperfine

    # Git
    abbr -a -U -- ga 'git add'
    abbr -a -U -- gb 'git branch -av'
    abbr -a -U -- gt 'git tag -ln'
    abbr -a -U -- gcm 'git commit -m'
    abbr -a -U -- gco 'git checkout'
    abbr -a -U -- gc 'git clone'
    abbr -a -U -- gd 'git diff'
    abbr -a -U -- gh 'git head'
    abbr -a -U -- gl 'git log --stat'
    abbr -a -U -- gp 'git pull'
    abbr -a -U -- gsl 'git log --graph --decorate --pretty=oneline --abbrev-commit --all'
    abbr -a -U -- gst 'git status -sb'
    abbr -a -U -- gwd git\ diff\ --word-diff-regex=\'\\w+\'

    # Docker
    abbr -a -U -- dk docker
    abbr -a -U -- dkp 'docker pull'
    abbr -a -U -- dkr 'docker run -it --rm'
    abbr -a -U -- dks 'docker search'
    abbr -a -U -- dki 'docker inspect'
    abbr -a -U -- dkip "docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}'"
    abbr -a -U -- dkc docker-compose

    # Web
    abbr -a -U -- dl 'wget -c'
    abbr -a -U -- wcat 'wget -q -O -'
    abbr -a -U -- yt 'youtube-dl -tic'
    abbr -a -U -- yta 'youtube-dl -ticx'
    abbr -a -U -- ports 'ss -tulepn'

    # Logs
    abbr -a -U -- dm 'dmesg --time-format=reltime -x'
    abbr -a -U -- dmw 'dmesg --time-format=reltime -xw'
    abbr -a -U -- bootlog 'journalctl -b'
    abbr -a -U -- jc 'journalctl -e'
    abbr -a -U -- jcf 'journalctl -f'

    # Time
    abbr -a -U -- now 'date +%Y-%m-%dT%H-%M-%S'
    abbr -a -U -- today 'date +%Y-%m-%d'
    abbr -a -U -- ut unix_time
    abbr -a -U -- ti time

    # System
    abbr -a -U -- sd sudo
    abbr -a -U -- sc 'sudo systemctl'
    abbr -a -U -- scs 'systemctl status'
    abbr -a -U -- scu 'systemctl --user'
    abbr -a -U -- scsu 'systemctl --user status'

    # System: NixOS
    abbr -a -U -- hm 'home-manager'
    abbr -a -U -- hms 'home-manager switch'
    abbr -a -U -- hme 'home-manager edit'
    abbr -a -U -- hmg 'home-manager generations'
    abbr -a -U -- ns 'nix search nixpkgs'
    abbr -a -U -- no 'nixos-option'
    abbr -a -U -- ngc 'sudo nix-collect-garbage --delete-older-than 10d'
    abbr -a -U -- upgrade 'sudo nixos-rebuild switch --upgrade'
    abbr -a -U -- ssw 'sudo nixos-rebuild switch'

    # Miscellaneous
    abbr -a -U -- b32 base32
    abbr -a -U -- b64 base64
    abbr -a -U -- md5 md5sum
    abbr -a -U -- sha sha256sum
    abbr -a -U -- pf 'printf "%s\n"'
    abbr -a -U -- sort 'sort -h'
    abbr -a -U -- rsort 'sort -hr'
    abbr -a -U -- pick 'pastel pick'
    abbr -a -U -- restart 'exec fish'

    # Set initialization flag
    set -U fish_abbreviations_initialized 1
end
