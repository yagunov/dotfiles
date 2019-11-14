function la --description "Simple list of directory's content, including hidden files and directories"
    command exa --group-directories-first -a $argv
end
