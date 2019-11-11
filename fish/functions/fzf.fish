function fzf --wraps fzf --description "FZF wrapper that puts choices to clipboard"
    # Make temporary file for choices
    set -l temp_file (mktemp /tmp/.fzf.XXXXXXXX)

    if command fzf $argv > $temp_file
        # Save selection to clipboard before dumping it to stdout
        set -l choices (command cat $temp_file)
        command rm $temp_file

        if test (count $choices) -eq 1
            echo -n "$choices" | xsel -i
        else
            printf "%s\n" $choices | xsel -i
        end

        printf "%s\n" $choices
    else
        # Fzf terminated with failure status, propagate it to the caller
        set -l fzf_status $status
        command rm $temp_file
        return $fzf_status
    end
end
