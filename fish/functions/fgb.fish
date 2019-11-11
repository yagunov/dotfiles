function fgb --description "Fuzzy Git Branch selection"
    set -l branch (command git branch --color=always \
                                      --all          \
                                      --verbose      \
                   | command fzf --no-multi          \
                   | sed -r 's/^\s*\*?\s+(\S+)\s*.*$/\1/')
    test -z "$branch" && return 1
    echo -n "$branch" | xsel -i
    echo $branch
    git checkout "$branch"
end
