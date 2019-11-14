function v --description "Edit in vim (file/directory/grep)"
    argparse --name=v "h/help" "f/file" "p/pattern" -- $argv
    or return
    if set -q _flag_help || set -q _flag_file _flag_pattern
        echo "Usage: v [OPTIONS] ARG ..."
        echo
        echo -- "Options:"
        echo -- "  -h | --help      Show this help message"
        echo -- "  -f | --file      File mode (interpret ARGs as filenames)"
        echo -- "  -p | --pattern   Pattern mode (interpret ARGs as a pattern and search current directory for it)"
        echo
        echo -- "File and pattern modes are mutually exclusive."
        echo
        return
    end

    if count $argv > /dev/null
        # Determine operation mode
        set -l file_mode (set -q _flag_file && echo on)
        or if not set -q _flag_pattern && test -e $argv[1]
            set file_mode on
        end

        if test -n "$file_mode"
            # Use case 1: 'v file_name ...' - open files or directories
            vim (test (count $argv) -le 3 && echo -- -o) $argv
        else
            # Use case 2: 'v not a file name' -> grep by pattern and select file to edit with fzf
            set -l place (command rg -i --color=always --line-number "$argv" | command fzf --no-multi -1 | string split :)
            or return
            vim $place[1] +$place[2]
        end
    else
        # Use case 3: 'v' -> 'v .'
        vim .
    end
end
