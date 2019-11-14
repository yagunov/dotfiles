function fe --description "Find files by extension"
    command fd --threads=4 --type=file --extension $argv
end
