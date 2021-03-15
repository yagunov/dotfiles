function gr --description "Change directory to the root of current git repositiry"
    cd (git rev-parse --show-toplevel)
end
