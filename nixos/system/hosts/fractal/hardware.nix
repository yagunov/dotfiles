# Hardware related configuration for my main machine.
#
# CPU: AMD Threadripper 3960X (24-cores, 48-threads)
# MEM: Kingston HyperX Predator [HX436C18PB3K4/128] 128 GB (4x32GB)
# MB: ASUS ROG Strix TRX40-E Gaming
# GPU: ASUS TUF Radeon RX5600XT EVO OC TUF-3
#
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" "amdgpu" ];
  boot.extraModulePackages = [ ];

  # System located on 1TiB Samsung 980 Pro NVMe SSD
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9cec3435-8a29-4dfc-95ed-ed6913d11d4e";
    fsType = "btrfs";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/D830-2BA7";
    fsType = "vfat";
  };

  # Sabrent 2TiB Rocket 4+ NVMe SSD used as a home partition
  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/44357cb6-ce84-4b42-ab26-8ae141485ebe";
    fsType = "btrfs";
  };

  # Backups and file storage
  fileSystems."/media/Hitachi-3TB" = {
    device = "/dev/disk/by-uuid/914163d1-122e-4389-b52e-bf53fb881c6b";
    fsType = "ext4";
  };
  fileSystems."/media/WD-Red-6TB" = {
    device = "/dev/disk/by-uuid/029e8a8d-ee66-4646-a8f9-25b8a96d41fa";
    fsType = "ext4";
  };

  # No swap partion used due to a large size of RAM
  swapDevices = [ ];

  # Store temporary files in RAM to increase SSD livetime
  boot.tmpOnTmpfs = true;

  # High-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Configure GPU
  hardware.opengl = {
    driSupport = true;
    extraPackages = with pkgs; [ rocm-opencl-icd rocm-opencl-runtime ];
  };
  services.xserver.videoDrivers = [ "amdgpu" ];
}
