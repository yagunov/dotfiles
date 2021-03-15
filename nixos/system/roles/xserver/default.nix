{ config, pkgs, lib, ... }:

{
  # Enable sound
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    # NixOS allows either a lightweight build (default) or full build of PulseAudio to be installed. Only the
    # full build has Bluetooth support, so it must be selected here.
    package = pkgs.pulseaudioFull;
  };

  # Enable hardware accelerated video decoding
  hardware.opengl.enable = true;

  # Choose layout switching method
  i18n.inputMethod.enabled = "fcitx5";

  # GUI fonts
  fonts.fonts = with pkgs; [
    jetbrains-mono
    vistafonts # mostly for Consolas
    nerdfonts # Collection of patched fonts
    cascadia-code # Windows Terminal font
    dejavu_fonts
    ubuntu_font_family
  ];

  # Virtualization
  virtualisation = {
    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };
  };

  # Configure PAM
  security.pam.services = {
    lightdm.enableGnomeKeyring = true;
    lightdm.sshAgentAuth = true;
    login.sshAgentAuth = true;
    login.enableGnomeKeyring = true;
    passwd.sshAgentAuth = true;
    passwd.enableGnomeKeyring = true;
  };

  # Environment variables
  environment.variables = {
    # Put programs cache in RAM
    XDG_CACHE_HOME = "$XDG_RUNTIME_DIR/cache";
    # Use gnome-keyring for SSH key management
    SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/keyring/ssh";
    # Select browser for opening surfraw search results
    SURFRAW_browser = "${pkgs.firefox}/bin/firefox";
  };

  # Gnome's passwords and keys manager
  programs.seahorse.enable = true;

  # Configure GPG agent
  programs.gnupg.agent = {
    enable = true;
    enableBrowserSocket = true;
    enableExtraSocket = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  # Qt5 Configuration Tool
  programs.qt5ct.enable = true;

  # Use wireshark with Qt-based frontend
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };

  nixpkgs.overlays = [
    # Use latest Emacs
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  # Additional packages to install
  environment.systemPackages = with pkgs; [
    # Desktop environment
    alacritty # main terminal
    compton # composition manager
    dmenu
    dunst # notifications
    fcitx-configtool
    gnome3.networkmanagerapplet
    i3lock # default i3 screen locker
    i3status
    i3status-rust # status bar generator (replacement for i3status)
    kitty # scratchpad terminal (alacritty has a problem with monitor switching)
    libnotify
    libsForQt5.fcitx-qt5
    lxappearance
    nitrogen # wallpaper manager
    numlockx # NumLock control
    parcellite # clipboard manager
    pasystray # PulseAudio systray
    rofi # Application launcher
    rofi-pass
    sakura # main terminal
    shutter # screanshot taker
    unclutter-xfixes # hide mouse cursore when idle
    wmfocus # select window from keyboard
    xorg.xev
    xorg.xkill
    xorg.xmodmap
    xss-lock
    pavucontrol # GUI volume control
    ponymix # CLI volume control
    xsel

    # Themes
    nordic
    nordic-polar
    paper-icon-theme

    # Web browsers
    chromium
    firefox
    qutebrowser
    tor-browser-bundle-bin

    # Development environment
    emacsUnstable
    kcachegrind
    massif-visualizer
    gitg
    gnome3.meld

    # Messengers
    discord

    # Digital graphics
    blender
    inkscape
    gimp-with-plugins

    # Office
    libreoffice

    # Multimedia players
    mpv
    ffmpeg-full
    vlc

    # Miscellanies tools
    ark # Archive manager
    glxinfo
    gparted # parted GUI
    psensor # GUI for lm_sensors (temperature plots)
    sqlitebrowser # SQLite GUI
    zathura # PDF viewer
    transmission-gtk # Torrent client
    googleearth
    sxiv # Image viewer
    teensy-loader-cli # ErgoDox firmware flasher (TODO: add udev rules)

    # Games
    gnome3.aisleriot
    gnome3.gnome-chess
    gnuchess
    stockfish
  ];

  # Configure desktop environment
  services.xserver = {
    enable = true;

    layout = "us,ru";

    # i3 tiling window manager
    displayManager.gdm.enable = true;
    desktopManager.gnome3.enable = true;
    displayManager.defaultSession = "none+i3";
    windowManager.i3.enable = true;
  };

  # Use Tor
  services.tor = {
    enable = true;
    client.enable = true;
  };

  # Bluetooth manager
  services.blueman.enable = true;

  # Enable automatic discovery of the printer from other Linux systems with avahi running.
  services.avahi = {
    enable = true;
    publish.enable = true;
    publish.userServices = true;
  };

  # Printing
  services.printing.enable = true;
}
