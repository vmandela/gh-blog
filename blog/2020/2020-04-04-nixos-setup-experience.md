---
title: NixOS configuration on a T480
tags: nixos, t480
---

After years of using nixpkgs on ubuntu and nixos in virtual machine,
I finally made the switch to using nixos as the primary OS on the 
home laptop. 
As the home laptop is shared, I used a dual boot configuration with
Windows10 along side NixOS.
Each OS is installed to a separate hard disk.
The installation process was pleasantly uneventful.

1. The WiFi card was autodetected. WiFi configuration was easy once I
   discovered `nmcli`.

    ~~~
    # /etc/nixos/configuration.nix
    networking.networkmanager.enable = true; # Enable network manager
    ~~~

    ~~~{.bash}
    $ nmcli d wifi connect vmandela --ask
    ~~~

2. With grub os prober enabled, windows 10 showed up automatically
   in the bootloader menu.

    ~~~
    # /etc/nixos/configuration.nix
    # Use the systemd-boot EFI boot loader.
    boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
            grub = {
                    enable = true;
                    devices = [ "nodev" ];
                    efiSupport = true;
                    useOSProber = true;
            };
    };
    ~~~

3. Windows 10 and NixOS treat the hardware clock differently.
   Windows 10 treats the hardware clock as local time while
   NixOS treats it as UTC.
   To resolve this difference, I had to switch NixOS to treat
   hardware clock as local time.

    ~~~
    # /etc/nixos/configuration.nix
    # Set your time zone.
    time.timeZone = "Asia/Kolkata";
    # Use hardware clock in local time instead of UTC
    # This is required for compatibility with windows
    time.hardwareClockInLocalTime = true;
    ~~~

4. [Steam](https://store.steampowered.com/) worked out of the box following the
   instructions in the [Nixpkgs Manual](https://nixos.org/nixpkgs/manual/#sec-steam-play).

    ~~~
    # /etc/nixos/configuration.nix
    # Required for steam
    hardware.pulseaudio.support32Bit = true;
    hardware.opengl.driSupport32Bit = true;
    ~~~

5. Setting up [NGINX](https://www.nginx.com/) required a little
   document hunting.
   I use NGINX for checking out my blog locally before pushing it
   to github.
   My usecase fell into the so simple it is undocumented category.

    ~~~
    # /etc/nixos/configuration.nix
    # Enable NGINX
    services.nginx = {
        enable = true;
        virtualHosts."localhost" = {
            root = "/home/vmandela/code/gh-blog/_site";
        };
    };
    ~~~

6. Power management and monitoring is done using `tlp` and `upower`.

    ~~~
    # For thinkpad
    services.tlp.enable = true;

    # Battery power management
    services.upower.enable = true;
    ~~~

7. Getting the user environment setup was done by reusing dotfiles
   from the previous machine and
   using [Nix Home Manager](https://github.com/rycee/home-manager)
