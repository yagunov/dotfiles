function b16 --description "Base16 theme selector"
    # Parse command line arguments
    argparse --name=b16 "r/random" "h/help" "l/list" "n/next" "p/previous" "U/update" -- $argv
    or return
    if test -n "$_flag_help"
        echo "Usage: b16 [-r|--random] [-h|--help] [-n|--next] [-p|--previous] [PATTERN] ..."
        echo
        echo -- "  -h | --help      Show this help message"
        echo -- "  -U | --update    Update local copy of `base16-shell` repository"
        echo -- "  -l | --list      Show list of available themes"
        echo -- "  -r | --random    Select random theme"
        echo -- "  -n | --next      Activate next theme (in alphanumeric order, patterns ignored)"
        echo -- "  -p | --previous  Activate previous theme (in alphanumeric order, patterns ignored)"
        echo
        return
    end

    # Choose themes repository location
    set -q __BASE16_REPO
    or begin
        if test -z "$XDG_DATA_HOME"
            set -U __BASE16_REPO "$HOME/.local/share/base16-shell"
        else
            set -U __BASE16_REPO "$XDG_DATA_HOME/base16-shell"
        end
    end
    set -l themes_path "$__BASE16_REPO/scripts/base16-"

    # Fetch themes repository when activated for the first time
    if test ! -e $__BASE16_REPO
        git clone https://github.com/chriskempson/base16-shell "$__BASE16_REPO"
        set -q _flag_update && return
    end

    # Update existing repository
    if set -q _flag_update
        git -C "$__BASE16_REPO" pull
        return
    end

    # Get list of all available BASE16 color schemes
    set -l themes (command ls $themes_path*.sh | sed "s!$themes_path\(.*\)\.sh\$!\1!")

    # Apply all name filters given by user unless called in interactive
    # selection mode
    if test -n "$_flag_list$_flag_random$_flag_next$_flag_previous"
        for pattern in $argv
            set themes (string match "*$pattern*" $themes)
        end
    end

    set -l selected_theme

    # Perform action based on CLI arguments
    if set -q _flag_list
        printf "%s\n" $themes
        return

    else if set -q _flag_random
        set selected_theme (random choice $themes)

    else if set -q _flag_next
        if set -q BASE16_THEME
            # Take next theme
            set -l idx (contains -i $BASE16_THEME $themes)
            if test $idx -eq (count $themes)
                set selected_theme $themes[1]
            else
                set selected_theme $themes[(math "$idx + 1")]
            end
        else
            # Start from the beginning of the theme list
            set selected_theme $themes[1]
        end

    else if set -q _flag_previous
        if set -q BASE16_THEME
            # Take previous theme
            set -l idx (contains -i $BASE16_THEME $themes)
            if test $idx -eq 1
                set selected_theme $themes[(count $themes)]
            else
                set selected_theme $themes[(math "$idx - 1")]
            end
        else
            # Start from the end of the theme list
            set selected_theme $themes[-1]
        end

    else
        # Let user interactively select theme
        set -l fzf_options '-1'
        if count $argv > /dev/null
            set -a fzf_options "--query" (string join ' ' (printf "'%s\n" $argv))" "
        end
        set selected_theme (printf "%s\n" $themes | fzf $fzf_options)

    end

    # Activate terminal theme if selected
    if test -n "$selected_theme"
      echo "Activating $selected_theme color theme"
      sh "$themes_path$selected_theme.sh"
      and set -g BASE16_THEME $selected_theme
    end
end
