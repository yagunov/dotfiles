function o --description "Open files/directories"
    set -l argc (count $argv)

    if test $argc -eq 0
        # Open current directory if called without arguments
        xdg-open .
    else if test $argc -le 10
        # Open all files
        for arg in $argv
            xdg-open $arg 2>&1 > /dev/null
        end
    else
        echo "Too many files. Intentionally limited to 10 in order to prevent accidental use of wildcards."
    end
end
