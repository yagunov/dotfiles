function diff --wraps diff --description "More readable diff"
    if status is-interactive
        # Called from shell directly
        command diff     \
            --new-file   \
            --text       \
            --unified=3  \
            --recursive  \
            --color=auto \
            $argv
    else
        # Called from external program, like `vimdiff`
        command diff $argv
    end
end
