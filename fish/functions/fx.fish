function fx --wraps fd --description "Find executable"
    command fd --threads=4 --type=executable $argv
end
