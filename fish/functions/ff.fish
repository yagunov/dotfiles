function ff --wraps fd --description "Find files"
    command fd --threads=4 --type=file $argv
end
