function ll --description "List content of directory using long format"
    exa --group-directories-first --time-style=long-iso --git -lh $argv
end
