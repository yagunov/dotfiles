# Hardware related configuration for my old machine.
#
# CPU: Intel Core i7-5820K (6-cores, 12-threads)
# MEM: Kingston HyperX 64 GB (Bx8GB)
# MB: ASUS X99-Delux
# GPU: Gegabyte GeForce GTX 970
#
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "ahci" "xhci_pci" "ehci_pci" "nvme" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "wl" ];
  boot.extraModulePackages = [ ];

  # NB: "intel_pstate=active" fixes poor CPU performance in 20.09 release
  # (https://discourse.nixos.org/t/browsers-unbearably-slow-after-update/9414/32)
  boot.kernelParams = [ "pci=noaer" "intel_pstate=active" ];

  # All system partitions located on 1TiB Samsung 960 Pro NVMe SSD
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/01b5e3a0-30ec-428a-824e-9e8829c945d6";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/8588-544D";
    fsType = "vfat";
  };

  # Swap on the same SSD
  swapDevices =
    [{ device = "/dev/disk/by-uuid/f1a47ec2-ab36-4983-8b14-d89e1f028f35"; }];

  # Store temporary files in RAM to increase SSD livetime
  boot.tmpOnTmpfs = true;

  # High-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Configure GPU
  hardware.opengl = {
    driSupport = true;
    extraPackages = with pkgs; [ vaapiVdpau ];
  };
  services.xserver.videoDrivers = [ "nvidia" ]; # chane to "nouveau"
}
