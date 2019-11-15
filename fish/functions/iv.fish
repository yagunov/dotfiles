function iv --description "Image viewer"
    switch (count $argv)
        case 0
            # Without argument just open current directory in thumbnail mode
            sxiv -qrt . 2>&1 > /dev/null &
        case 1
            if test -d $argv[1]
                sxiv -qrt $argv 2>&1 > /dev/null &
            else
                sxiv -qrb $argv 2>&1 > /dev/null &
            end
        case "*"
            sxiv -qrt $argv 2>&1 > /dev/null &
    end
end
