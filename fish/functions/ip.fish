function ip --wraps ip
    if count $argv > /dev/null
        command ip -color=auto $argv
    else
        command ip -color=auto -brief addr
    end
end
