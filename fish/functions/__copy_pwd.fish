function __copy_pwd --description "Copy current working directory to clipboard"
    echo -n (pwd) | xsel -i
end
