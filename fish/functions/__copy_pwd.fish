function __copy_pwd --description "Copy current working directory to clipboard"
    pwd | xsel -i
end
