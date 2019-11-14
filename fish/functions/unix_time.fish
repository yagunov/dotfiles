function unix_time --description "Convert time to unix time and back"
    argparse --name=unix_time "h/help" "t/to" -- $argv
    or return
    if set -q _flag_help
        echo "Usage: unix_time [OPTIONS] [PATTERN] ..."
        echo
        echo -- "Options:"
        echo -- "  -h | --help      Show this help message"
        echo -- "  -t | --to        Convert human readable date to Unix timestamp (default is reverse)"
        echo
    end

    if tty > /dev/null
        if count $argv > /dev/null
            for dt in $argv
                if set -q _flag_to
                    # convert human readable date to Unix timestamp
                    date -d "$dt" +'%s'
                else
                    # convert Unix timestamp to human readable date
                    date -d @$dt +'%Y-%m-%d %H:%M:%S'
                end
            end
        else
            date +%s             # display current time in Unix time
        end
    else
        while read line
            if set -q _flag_to
                # convert human readable date to Unix timestamp
                date -d "$line" +'%s'
            else
                # convert Unix timestamp to human readable date
                date -d @$line +'%Y-%m-%d %H:%M:%S'
            end

        end
    end
end
