with (import <nixpkgs> { });

let
  ghc = haskellngPackages;

  withHoogle = haskellEnv:
    ghc.callPackage <nixpkgs/pkgs/development/libraries/haskell/hoogle/local.nix> {
      packages = haskellEnv.paths;
    };

  ghcPackages = ghc.ghcWithPackages (p: with p; [
    ipprint
    #ghc-mod
    #hdevtools
    stylish-haskell
    cabal-install
    cabal2nix
    hakyll
  ]);

in

with pkgs;

runCommand "dummy" {
  buildInputs = [
    ghcPackages
    (withHoogle ghcPackages)
    pkgconfig
    pythonPackages.pygments
  ];
  shellHook = ''
    export NIX_GHC="${ghcPackages}/bin/ghc"
    export NIX_GHCPKG="${ghcPackages}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${ghcPackages}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
} ""
