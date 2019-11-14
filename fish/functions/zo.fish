function zo --description 'zo with fzf integration'
    if not count $argv >/dev/null
        xdg-open (__z -l | fzf | sed 's/^[0-9,.]* *//')
    else
        __z -d $argv
    end
end
