{ compiler    ? "ghc863"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "a3b6b49eac91baa25a01ef10b74a7aeb89a963a1"
, sha256      ? "1za2mvmc9nlxb91syd608fjrkm53rm658nflracy1js1a3nlaj06"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    inherit doBenchmark;
  });

  inherit returnShellEnv;
}
