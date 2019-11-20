if status is-interactive; and not set -Uq fish_abbreviations_initialized
    # - to cd to previously visited directory
    abbr -a -U -- - 'cd -'

    # Files: listing
    abbr -a -U -- all lla
    abbr -a -U -- tta 'tt --all'
    abbr -a -U -- ta 't --all'
    abbr -a -U -- du 'du -sh'
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

    # Files: search
    abbr -a -U -- fai 'fa -IH'  # -IH means --no-ignore --hidden
    abbr -a -U -- fdi 'fd -IH'
    abbr -a -U -- fei 'fe -IH'
    abbr -a -U -- ffi 'ff -IH'
    abbr -a -U -- fli 'fl -IH'
    abbr -a -U -- fxi 'fx -IH'
    abbr -a -U -- g 'rg -i'
    abbr -a -U -- gi 'rg -iv'
    abbr -a -U -- gg 'rg -i --no-ignore --hidden'

    # Development
    abbr -a -U -- cg cargo
    abbr -a -U -- d diff
    abbr -a -U -- mk 'make -j12'
    abbr -a -U -- vd vimdiff
    abbr -a -U -- man fman

    # Git
    abbr -a -U -- ga 'git add'
    abbr -a -U -- gb 'git branch -av'
    abbr -a -U -- gc 'git commit -m'
    abbr -a -U -- gcl 'git clone'
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

    # Web
    abbr -a -U -- dl 'wget -c'
    abbr -a -U -- wcat 'wget -q -O -'
    abbr -a -U -- yt 'youtube-dl -tic'
    abbr -a -U -- yta 'youtube-dl -ticx'

    # Logs
    abbr -a -U -- dm 'dmesg --time-format=reltime -x'
    abbr -a -U -- dmw 'dmesg --time-format=reltime -xw'

    # Time
    abbr -a -U -- now 'date +%Y-%m-%dT%H-%M-%S'
    abbr -a -U -- today 'date +%Y-%m-%d'
    abbr -a -U -- ut unix_time

    # System
    abbr -a -U -- sd sudo
    abbr -a -U -- sc 'sudo systemctl'
    abbr -a -U -- scu 'systemctl --user'

    # Miscellaneous
    abbr -a -U -- b32 base32
    abbr -a -U -- b64 base64
    abbr -a -U -- sort 'sort -h'
    abbr -a -U -- rsort 'sort -hr'
    abbr -a -U -- pick 'pastel pick'
    abbr -a -U -- restart 'exec fish'

    # Set initialization flag
    set -U fish_abbreviations_initialized 1
end
