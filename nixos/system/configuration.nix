{ config, pkgs, lib, ... }:

{
  # Load additional modules
  imports = [
    /etc/nixos/host/configuration.nix
    /etc/nixos/host/hardware.nix
    ./roles/terminal
    ./roles/xserver
  ];

  # Allow installation of unfree software (needed for nvidia drivers)
  nixpkgs.config.allowUnfree = true;

  # Enable Nix Flakes
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Basic console configuration
  boot.loader.systemd-boot.consoleMode = "max";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Use latest available kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Select internationalisation properties
  i18n.defaultLocale = "en_US.UTF-8";

  # Basic networking
  networking = {
    # Configure DHCP per-interface in host related configuration
    useDHCP = false;
    # Always use firewall
    firewall.enable = true;
    # Enable network manager on systems with GUI
    networkmanager.enable = config.services.xserver.enable;
  };

  # I use fish for my user
  programs.fish = {
    enable = true;
    shellAliases = lib.mkForce { }; # remove default aliases
  };

  # Don't forget to set a password with ‘passwd’ on newly installed systems
  users.users.andrey = {
    isNormalUser = true;
    uid = 1000;
    shell = "${pkgs.fish}/bin/fish";
    extraGroups = [
      "wheel"
      "audio"
      "networkmanager"
      "vboxusers"
      "docker"
      "wireshark"
      "scanner"
      "lp"
    ];
  };

  # This value determines the NixOS release from which the default settings for stateful data, like file
  # locations and database versions on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system. Before changing this value read the
  # documentation for this option (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09";
}
