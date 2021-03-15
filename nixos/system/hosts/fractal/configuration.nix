{ config, pkgs, lib, ... }:

{
  # Assign hostname
  networking.hostName = "fractal";

  # Set local time zone
  time.timeZone = "Asia/Irkutsk";

  # Allow DHCP on these interfaces
  networking.interfaces.enp67s0.useDHCP = true;
  networking.interfaces.enp68s0.useDHCP = true;

  # Enable printing (for Epson L355)
  services.printing.drivers = [ pkgs.epson_201207w ];

  # Scanner support (Epson L355)
  hardware.sane = {
    enable = true;
    extraBackends = [
      (pkgs.epkowa.overrideAttrs (oldAttr: {
        postInstall = (oldAttr.postInstall or "") + ''
          echo "net 192.168.88.5" >> $out/etc/sane.d/epkowa.conf
        '';
      }))
    ];
  };
  # TODO: Enable scanning from GIMP
  # environment.systemPackages = [
  #   pkgs.xsane.override { gimpSupport = true; }
  # ];

  # Share files with windows
  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = ${config.networking.hostName} server (Samba, NixOS)
      security = user
      use sendfile = yes
      hosts allow = 192.168.88.0/24 localhost
      hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
    '';
    shares = {
      "Nika's Backup" = {
        path = "/media/WD-Red-6TB/Shares/Nika's Backup";
        browseable = "yes";
        writable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0666";
        "directory mask" = "0777";
      };
    };
  };
  # Open SAMBA ports
  networking.firewall.allowedTCPPorts =
    if config.services.samba.enable then [ 445 139 ] else [ ];
  networking.firewall.allowedUDPPorts =
    if config.services.samba.enable then [ 137 138 ] else [ ];

  # Monitor setup
  services.xserver.xrandrHeads = [
    {
      output = "DisplayPort-3";
      monitorConfig = ''Option "Rotate" "right"'';
    }
    {
      output = "DisplayPort-1";
      primary = true;
      monitorConfig = ''Option "Position" "1440 320"'';
    }
  ];

}
