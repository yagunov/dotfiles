function fe --wraps fd --description "Find files by extension(s)"
    set -l arguments (string match -e -- '-' $argv)
    set -l extensions (string match -ev -- '-' $argv)
    command fd --threads=4 --type=file $arguments --extension=$extensions
end
