function lsusb --wraps lsusb
    if not count $argv > /dev/null
        command lsusb -t $argv
    else
        command lsusb $argv
    end
end
