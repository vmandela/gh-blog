{
  description = "Flake for blog using Hakyll";

  # use nixpkgs 22.11
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.default =
      # Notice the reference to nixpkgs here.
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "gh-blog";
        src = self;
        buildDepends = [
                     pkgs.haskellPackages.cabal-install
                     pkgs.haskellPackages.pandoc
                     pkgs.haskellPackages.hakyll
                     pkgs.haskellPackages.raw-strings-qq
                     ];
      };

    # devShell = [
    #                  nixpkgs.haskellPackages.cabal-install
    #                  nixpkgs.haskellPackages.pandoc
    #                  nixpkgs.haskellPackages.hakyll
    #                  nixpkgs.haskellPackages.raw-strings-qq
    #                  ];

  };
}
