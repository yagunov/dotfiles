{ pkgs, ... }:

{
  home.packages = with pkgs; [
    rust-analyzer
    inotify-tools

    # Work related
    citrix_workspace_21_01_0
    remmina
  ];

  services.lorri.enable = true;

  gtk = {
    enable = true;
    # UI theme
    theme.name = "Nordic";
    theme.package = pkgs.nordic;
    # Icon theme
    iconTheme.name = "kora-pgrey";
    iconTheme.package = pkgs.kora-icon-theme;
    # Font
    font.name = "Ubuntu 10";
    font.package = pkgs.ubuntu_font_family;
  };

  qt = {
    enable = true;
    # Use same theme as GTK applications
    platformTheme = "gtk";
    style.package = pkgs.nordic;
    style.name = "Nordic";
  };
}
