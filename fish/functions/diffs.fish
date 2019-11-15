function diffs --description "Smart diffstat"
    if tty > /dev/null
        if count $argv > /dev/null
            diff $argv | command diffstat -C
        else
            # Use case 2: called without argument (will not work outside of git repository)
            command git diff | command diffstat -C
        end
    else
        # Use case 3: input is a pipe
        command diffstat -C
    end
end
