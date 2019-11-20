function fd --wraps fd --description "Find directories"
    command fd --threads=4 --type=directory $argv
end
