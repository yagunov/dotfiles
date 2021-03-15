{ config, pkgs, lib, ... }:

{
  # Virtualization
  virtualisation.docker.enable = true;

  # Documentation
  documentation = {
    enable = true;
    man.enable = true;
    nixos.enable = true;
    dev.enable = true;
  };

  # Configure SSH
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
  };

  # traceroute + ping
  programs.mtr.enable = true;

  # System-wide custom (neo)vim configuration
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    # Plugins loaded on launch
    configure.packages.myVimPackages.start = with pkgs.vimPlugins; [
      # UI ---------------------------------------------------------
      lightline-vim
      papercolor-theme

      # Editor features --------------------------------------------
      commentary # gcap
      easy-align # vipga
      fzf-vim
      neoformat
      nerdtree
      surround # cs"
      vim-gitgutter
      vim-indent-object # >aI

      # Syntax/language support ------------------------------------
      Jenkinsfile-vim-syntax
      meson
      rust-vim
      vim-fish
      vim-flatbuffers
      vim-go
      vim-nix
      vim-orgmode
      vim-toml
    ];

    configure.customRC = builtins.readFile ./vim/config.vim;
  };

  # Additional packages to install
  environment.systemPackages =
    let home-manager = (import <home-manager> { }).home-manager;
    in with pkgs; [
      # Nix related stuff
      home-manager
      nixpkgs-fmt # Nix code formatter
      nixfmt # another Nix code formatter that can be used in pipes

      # Tools: system info
      inxi
      i2c-tools # decode-dimms
      usbutils # lsusb
      pciutils # lspci

      # Tools: monitoring and benchmarks (See: https://habr.com/ru/post/476414/)
      sysstat
      dstat
      lsof
      iotop
      iftop
      iperf
      blktrace # btrace
      fatrace
      bpftool
      lm_sensors
      linuxPackages.perf
      linuxPackages.bcc
      linuxPackages.bpftrace
      fio # Flexible I/O Tester (see `man iowatcher`)

      # Tools: JSON
      jq
      jo

      # Tools: image manipulation
      imagemagick
      pngquant # PNG optimization
      jpegoptim # JPEG optimization

      # Tools: password management
      pass
      pwgen
      pwgen-secure # TODO: password length does not work in pwgen-secure?

      # Tools: shell
      file
      tmux
      starship
      bat
      exa
      fd
      ripgrep
      dua
      ncdu # interactive disk usage browser
      fzf
      httpie
      wget
      socat
      websocat
      surfraw
      youtube-dl
      xsv
      hyperfine
      hexyl
      htop
      diffstat
      pv
      vifm
      tokei
      fpp # Facebook PathPicker
      pastel # TODO: requires xcolor or gpick (both are missing)
      ncat # nmap
      whois
      gcal # calendar
      p7zip
      unrar
      parallel
      parted
      sshfs
      vivid

      # Documentation
      manpages
      stdmanpages
      libcap_manpages
      stdman # C++17 stdlib reference
      tealdeer # rust client for tldr

      # Writing
      texlive.combined.scheme-full
      pdftk
      ghostscript
      pandoc
      graphviz

      # Development: DB clients
      mycli
      mysql-client
      pgcli
      sqlite
      # TODO: add clickhouse

      # Development: git
      gitAndTools.gitFull
      gitAndTools.gitflow # TODO: see http://www.git-town.com/ (nixos.git-town)
      gitAndTools.tig
      gitAndTools.delta

      # Development: editors and IDE
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      aspellDicts.ru
      direnv
      lorri
      clang-tools

      # Development: virtualization and containers
      docker-compose

      # Development: native build environment
      binutils
      gnumake
      ninja
      meson
      cmake
      cmakeCurses
      gcc10
      clang_10

      # Development: native debug and performance analyzes
      gdb
      lldb
      valgrind
      heaptrack

      # Development: rust
      rustup
      cargo-asm
      cargo-audit
      cargo-bloat
      cargo-deps
      cargo-expand
      cargo-generate
      cargo-inspect
      cargo-edit
      cargo-flamegraph

      # Development: go
      go
      gopls
      dep

      # Development: python
      pipenv
      poetry
      black # Python code formatter
      python39Full
      python39Packages.ipython
      # TODO: python39Packages.matplotlib.override { enableGtk3 = true; }
    ];
}
