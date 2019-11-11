function mpv --wraps mpv --description "MPV wrapper with automatic loading of external media files"
    set -l search_patterns
    set -l sub_paths
    set -l audio_paths

    function find_directories
        command fd --type=directory --ignore-case --max-depth=1 $argv
    end

    # Only attach external audio paths when they are not specified by the user
    not contains -- '--audio-file-paths' $argv
    and set -a search_patterns "sound"

    # Only attach external subtitles paths when they are not specified by the user
    not contains -- '--sub-file-paths' $argv
    and set -a search_patterns "sub"

    # Search for additional media files paths
    count $search_patterns > /dev/null
    and for dir in (find_directories (string join '|' $search_patterns))
        switch (string lower $dir)
            case "*sub*"
                set -a sub_paths $dir
                set -a sub_paths (find_directories --search-path $dir)
            case "*sound*"
                set -a audio_paths $dir
                set -a audio_paths (find_directories --search-path $dir)
        end
    end

    # Build MPV option list
    set -l options --audio-channels=stereo \
        --audio-file-auto=fuzzy            \
        --no-audio-display                 \
        --sub-auto=fuzzy                   \
        --alang=eng,rus                    \
        --slang=eng,rus                    \
        --sub-use-margins                  \
        --no-sub-visibility

    # Add new subtitles paths
    count $sub_paths > /dev/null
    and set -a options --sub-file-paths=(string join ':' $sub_paths)
    # Add new audio file paths
    count $audio_paths > /dev/null
    and set -a options --audio-file-paths=(string join : $audio_paths)

    command mpv $options $argv
end
