function lla --description "List content of directory using logn format, including hidden files and directories"
    exa --group-directories-first --time-style=long-iso --git -lha $argv
end
