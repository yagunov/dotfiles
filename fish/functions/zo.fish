function zo --description 'zo with fzf integration'
    if not count $argv >/dev/null
        set -l dest (__z -l | fzf | sed 's/^[0-9,.]* *//')
        test -n "$dest" && xdg-open $dest
    else
        __z -d $argv
    end
end
