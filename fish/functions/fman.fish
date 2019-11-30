function fman --wraps man --description "Fuzzy man-page search"
    set -l man_pager "bat --language man --plain"
    if count $argv > /dev/null
        man -P $man_pager $argv
    else
        set -l topic (man -k . | fzf | cut -d ' ' -f 1 | sed -r "s!^(\S+)\s+\((.*)\).*!\1\n\2!")
        count $topic > /dev/null
        and man -P $pager $topic[2] $topic[1]
    end
end
