function tt --description "Filesystem tree in long format, full depth"
    set -l default_options --group-directories-first \
                           --time-style=long-iso     \
                           --git                     \
                           -lhT

    # Only automatically exclude files if there is no `--all` option
    if not contains -- -a $argv && not contains -- --all $argv
        set -l exclude_patterns '*~' '__pycache__'

        # Ignore target directory unless it is directly specified on command line
        test -e Cargo.toml
        and not string match -- "target" $argv > /dev/null
        and set -a exclude_patterns "target"

        # Ignore build directories unless they are directly specified on command line
        test -e CMakeLists.txt
        and not string match -- "build*" $argv > /dev/null
        and set -a exclude_patterns 'build*'

        set -a default_options -I (string join '|' $exclude_patterns)
    end

    command exa $default_options $argv
end
