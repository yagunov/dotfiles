function z --description 'z with fzf integration'
    if not count $argv >/dev/null
        set -l dest (__z -l | fzf | sed 's/^[0-9,.]* *//')
        test -n "$dest" && cd $dest
    else
        __z $argv
    end
end
