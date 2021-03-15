{ pkgs, ... }:

let
  rust-analyzer-unwrapped = pkgs.callPackage <nixos/pkgs/development/tools/rust/rust-analyzer/generic.nix> rec {
    rev = "2021-02-08";
    version = "unstable-${rev}";
    sha256 = "sha256-Idaaw6d0lvBUyZxpHKQ94aMtgM0zb0P8QRh+3pctX4k=";
    cargoSha256 = "sha256-J6Hia83biutScZt/BMO4/qXYi35/Ec9MeaHeDG8Lqmc=";
    doCheck = false;
  };
in
{
  services.lorri.enable = true;

  home.packages = [
    rust-analyzer-unwrapped
    pkgs.citrix_workspace_21_01_0
  ];
}
