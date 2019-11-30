function diff --wraps diff --description "More readable diff"
    set -l options

    if status is-interactive
        # Called from shell directly
        set -a options --new-file --text --recursive --color=auto
        if not contains -- -y $argv && not contains -- --side-by-side $argv
            set -a options --unified=3
        end
    end

    command diff $options $argv
end
