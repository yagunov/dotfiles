function la --description "Simple list of directory's content, including hidden files and directories"
    exa --group-directories-first -a $argv
end
