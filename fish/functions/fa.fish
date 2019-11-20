function fa --wraps fd --description "Find files and directories"
    command fd --threads=4 $argv
end
