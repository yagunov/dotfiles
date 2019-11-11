function diff --wraps diff --description "More readable diff"
    command diff     \
        --new-file   \
        --text       \
        --unified=3  \
        --recursive  \
        --color=auto \
        $argv
end
