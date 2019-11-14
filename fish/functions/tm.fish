function tm --description "Simple Tmux manager"
    # Parse command line arguments
    argparse --name=tm "h/help" "l/list" "k/kill" -- $argv
    or return
    if set -q _flag_help
        echo "Usage: tm [-h|--help] [-l|--list] [-k|--kill] [SESSION_NAME] ..."
        echo
        echo -- "  -h | --help      Show this help message"
        echo -- "  -l | --list      Show list of available themes"
        echo -- "  -k | --kill      Kill selected tmux sessions (via command line or interactively)"
        echo
        echo -- "Session name(s) can be given via command line or selected interactively with fzf."
        echo -- "If SESSION_NAME does not exist it will be created (or ignored in case of `--kill`)"
        echo
        return
    end

    # List all available TMUX sessions
    if set -q _flag_list
        command tmux list-sessions
        return
    end

    # Kill session
    if set -q _flag_kill
        count $argv > /dev/null
        or set argv (command tmux list-sessions -F "#{session_name}" | command fzf --multi)
        for session in $argv
            command tmux kill-session -t $session
        end
        return
    end

    # Get session name
    set -l session $argv[1]
    test -z "$session" && set session (command tmux list-sessions -F "#{session_name}" | command fzf --no-multi -1)
    test -z "$session" && return

    # Choose session change command
    set -l change_cmd attach-session
    set -q TMUX && set -l change_cmd switch-client

    # if command tmux has-session -t $argv[1]
    command tmux $change_cmd -t $session 2> /dev/null && return

    # Apply some session specific settings before creating new session
    switch $session
        case top
            b16 hopscotch
        case mid
            b16 material-darker
        case bot
            b16 material-palenight
        case scratch
            b16 material
    end

    # Create new session and attach it
    cd $HOME
    command tmux -2u new-session -d -s $session
    and command tmux $change_cmd -t $session
end
