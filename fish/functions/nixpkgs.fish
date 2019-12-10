function nixpkgs --description "cd into directory with current nixpkgs"
    set -l pkgs (string match -e 'nixpkgs=' $NIX_PATH)
    and set -l pkgs (string split = $pkgs)[2]
    and cd $pkgs
end
