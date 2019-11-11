function fgc --description "Fuzzy Git Checkout commit from history"
    set -l commit (command git log --color=always                                  \
                                   --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" \
                                   $argv                                           \
                   | command fzf --no-multi                                        \
                   | cut -d ' ' -f 1)
    test -z "$commit" && return 1
    echo -n "$commit" | xsel -i
    git checkout "$commit"
end
