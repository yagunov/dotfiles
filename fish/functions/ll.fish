function ll --description "List content of directory using long format"
    command exa                   \
        --group-directories-first \
        --time-style=long-iso     \
        --git                     \
        --grid                    \
        --long                    \
        --header                  \
        $argv
end
