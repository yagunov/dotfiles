function __fish_sink_output --description 'Sink output of current command to /dev/null'
    if test -z (commandline -j)
        commandline -a $history[1]
    end

    if commandline -j | string match -q -r -v "2>&1 > /dev/null\$"
        commandline -aj " 2>&1 > /dev/null;"
    end
end
