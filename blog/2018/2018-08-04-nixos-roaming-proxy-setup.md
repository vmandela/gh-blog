---
title: NixOS network proxy setup when roaming
tags: nixos
---

**Note** : This post assumes familiarity with NixOS <https://nixos.org>

One of the problems I faced when using NixOS is network access when moving my
laptop between work environment and home environment. The office network
needs a proxy for internet access while the home network is a direct
connection.

Normally this is not a problem. I can set the proxy variable in the
shell I am in.

~~~{.bash}
$ export HTTP_PROXY=http://corporate.proxy.com:port
$ wget www.google.com
~~~

However this gets problematic when trying to install a new package. Package
updates are performed by the Nix daemon. The proxy settings for Nix daemon
are derived from nixos configuration [2].

## Update configuration with the new proxy setting

The first solution that I tried was updating nixos configuration after switching
networks and rebuilding the configuration. This got tedious quickly. On top of
it, rebuilding configuration without network access could fail [3].

## Other Options

After digging a bit more, I came across 3 more options.

1. Child configurations - These are new configurations that are built each time
   a nixos configuration is built. These are treated as a separate configuration
   from the base configuration, almost as a peer to it.

   See here <https://nixos.org/nixos/manual/options.html#opt-nesting.clone> for
   more information.

2. Clone configurations - These are also new configurations that are built each
   time a nixos configuration is built but are treated as variations on top of
   a base configuration. This is the most suitable option for my usecase.

   See here <https://nixos.org/nixos/manual/options.html#opt-nesting.children> for
   more information.

3. Profiles - Profiles are system configurations that need to be explicitly
   built and are maintained separately from the default boot configuration. This
   is not suitable for my usecase as the configuration needs to be manually
   built to keep it up to date.

I decided to try the clone configuration as it was most suited to my usecase.

## Clone configuration

Adding the clone configuration to `/etc/nixos/configuration.nix` was straight forward.

~~~
boot.loader.grub.configurationName = "Default";
nesting.clone = [
        {
                boot.loader.grub.configurationName = lib.mkForce "Work";
                networking.proxy.default = "http://proxy.work.com:80";
                networking.proxy.noProxy = "127.0.0.1,localhost,work.com";
         }
]
~~~

After building the configuration once with

~~~
$ sudo nixos-rebuild switch
~~~

I could switch the proxy settings easily by switching from the parent
configuration to the cloned configuration.

~~~
$ sudo /run/current-system/fine-tune/child-1/bin/switch-to-configuration test
~~~

One minor inconvenience remained. I had no way of selecting the proxy
configurations on boot. I discovered that this functionality existed at some
point and was temporarily disabled during refactoring. I enabled this
feature and filed a PR on Nixpkgs.

<https://github.com/NixOS/nixpkgs/pull/45345>

This PR took almost an year to get merged due to the iterations on test cases
and delays due to my work commitments. You should be able to select different
proxy configurations from the boot loader menu from NixOS 19.09.

## References

1. roaming laptop: network proxy configuration - <https://github.com/NixOS/nixpkgs/issues/27535>
2. Nix-daemon proxy setup - <https://github.com/NixOS/nixpkgs/issues/27535#issuecomment-317719143>
3. [Nix-dev] nixos-rebuild without Internet - <https://nixos.org/nix-dev/2017-June/023965.html>
4. Support multiple configuration files (selectable from grub) - <https://github.com/NixOS/nixpkgs/issues/23458>
