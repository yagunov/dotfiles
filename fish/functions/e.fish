function e --description "Edit in emacs (file/directory/grep/stdin)"
    argparse --name=e "h/help" "f/file" "p/pattern" -- $argv
    or return
    if set -q _flag_help || set -q _flag_file _flag_pattern
        echo "Usage: e [OPTIONS] ARG ..."
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

    if tty > /dev/null
        if count $argv > /dev/null
            # Determine operation mode
            set -l file_mode (set -q _flag_file && echo on)
            or if not set -q _flag_pattern && test -e $argv[1]
                set file_mode on
            end

            # TODO: pattern: /path/to/file:XX[:XX]

            if test -n "$file_mode"
                # Use case 1: 'e file_name ...' - open files or directories
                emacsclient --no-wait $argv
            else
                # Use case 2: 'e not a file name' -> grep by pattern and select file to edit with fzf
                set -l place (command rg -i --color=always --line-number "$argv" | command fzf --no-multi -1 | string split :)
                or return
                command emacsclient --no-wait +$place[2] $place[1]
            end
        else
            # Use case 3: 'e' -> 'e .'
            emacsclient --no-wait .
        end
    else
        # Use case 4: 'some command | e' - Redirect STDIN to temporary file, edit it and dump edited text to STDOUT
        set -l temp_file (mktemp /tmp/.fish_stdin.XXXXXXXX)
        command cat > $temp_file
        command emacsclient $temp_file 2>&1 > /dev/null
        command cat $temp_file
        command rm $temp_file
    end
end
