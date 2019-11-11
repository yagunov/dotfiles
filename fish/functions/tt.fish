function tt --description "Filesystem tree in long format, full depth"
    set -l exclude_patterns '*~' '__pycache__'

    # Ignore target directory unless it is directly specified on command line
    test -e Cargo.toml
    and not string match -- "target" $argv > /dev/null
    and set -a exclude_patterns "target"

    # Ignore build directories unless they are directly specified on command line
    test -e CMakeLists.txt
    and not string match -- "build*" $argv > /dev/null
    and set -a exclude_patterns 'build*'

    exa --group-directories-first              \
        --time-style=long-iso                  \
        --git                                  \
        -lhT                                   \
        -I (string join '|' $exclude_patterns) \
        $argv
end
