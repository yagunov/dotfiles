function fl --wraps fd --description "Find symlinks"
    command fd --threads=4 --type=symlink $argv
end
