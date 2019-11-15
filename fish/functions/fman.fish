function fman --description "Fuzzy man-page search"
    if count $argv > /dev/null
        man $argv
    else
        set -l topic (man -k . | fzf | cut -d ' ' -f 1 | sed -r "s!^(\S+)\s+\((.*)\).*!\1\n\2!")
        count $topic > /dev/null
        and man $topic[2] $topic[1]
    end
end
