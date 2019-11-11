function z --description 'z with fzf integration'
    if not count $argv >/dev/null
        cd (__z -l | fzf | sed 's/^[0-9,.]* *//')
    else
        __z $argv
    end
end
