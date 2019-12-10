#!/bin/sh

DEFAULT_SESSION="$1"

if [ -z "$SSH_ORIGINAL_COMMAND" ] ; then
    echo "
Welcome to $USER@`hostname`.

What would you like to do:
[enter]  Connect to default tmux session
1        Start new tmux session
2        Start normal shell
3        Show htop
q        Quit
"
    choice=""
    while [ x"$choice" != x"q" ]; do
        echo -n "Your choice: "
        read choice
        case x"$choice" in
            x1)
                tmux -2u new-session
                exit 0
                ;;
            x2)
                zsh
                exit 0
                ;;
            x3)
                htop
                exit 0
                ;;
            xq|xQ)
                echo "Goodbye!"
                exit 0
                ;;
            x)
                tmux -2u new-session -s "${DEFAULT_SESSION}" || tmux -2u attach -t "${DEFAULT_SESSION}"
                exit 0
                ;;
            *)
                echo "Invalid choice '$choice': please try again"
        esac
    done
else
    eval "$SSH_ORIGINAL_COMMAND"
fi

exit 0
