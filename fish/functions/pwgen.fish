function pwgen --wraps pwgen
    if string match "1" $argv[-1] > /dev/null
        # When output will be a single password copy it into clipboard
        set -l pass (command pwgen --secure $argv)
        echo -n "$pass" | xsel -i
        echo "$pass"
    else if count $argv > /dev/null
        command pwgen --secure $argv
    else
        # Generate strong password and copy it into clipboard
        set -l pass (command pwgen -CBs 20 1)
        echo -n "$pass" | xsel -i
        echo "$pass"
    end
end
