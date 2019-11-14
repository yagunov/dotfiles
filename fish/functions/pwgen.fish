function pwgen --wraps pwgen
    # NOTE: fzf wrapper will copy selected password to clipboard.
    if contains -- -C $argv
        # Column output
        command pwgen --secure $argv
    else if count $argv > /dev/null
        command pwgen --secure $argv | fzf --no-multi -1
    else
        # Default: generate strong password
        command pwgen --secure --ambiguous 24 10 | fzf --no-multi -1
    end
end
