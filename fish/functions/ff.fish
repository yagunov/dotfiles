function ff --description "Find files"
    command fd --threads=4 --type=file $argv
end
