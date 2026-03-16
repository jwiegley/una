{
  description = "Universal unarchiving software";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };

      overlays = [
        haskellNix.overlay
        (final: prev: {
          una = final.haskell-nix.project' {
            src = ./.;
            supportHpack = true;
            compiler-nix-name = "ghc910";

            shell.tools = {
              cabal = {};
              haskell-language-server = {};
            };

            shell.buildInputs = with pkgs; [
              pkg-config
              hlint
              fourmolu
              lefthook
            ];
          };
        })
      ];

      flake = pkgs.una.flake {};

      # The main una executable package
      unaExe = flake.packages."una:exe:una";

      # Tools for checks
      hlint = pkgs.hlint;
      fourmolu = pkgs.fourmolu;

    in {

      # ---------------------------------------------------------------
      # Packages
      # ---------------------------------------------------------------

      packages = {
        default = unaExe;

        # Coverage build: builds with HPC enabled and produces a coverage
        # report by exercising findExtractors.
        coverage = pkgs.runCommand "una-coverage" {
          nativeBuildInputs = [
            (pkgs.haskellPackages.ghcWithPackages (hp: [
              hp.cmdargs hp.io-storage hp.directory hp.process
              hp.filepath hp.bytestring
            ]))
          ];
        } ''
          mkdir -p $out

          # Copy source
          cp ${self}/Main.hs .

          # Compile with HPC coverage enabled
          ghc -fhpc -threaded -o una-cov Main.hs \
            -hpcdir .hpc

          # Run a quick exercise of the binary (--help exits non-zero but
          # exercises module load; we just need the tix file generated)
          ./una-cov --help || true

          # Generate the coverage report
          hpc report una-cov.tix --hpcdir .hpc > $out/coverage-report.txt || true
          hpc markup una-cov.tix --hpcdir .hpc --destdir=$out/html || true

          echo "Coverage report written to $out"
        '';

        # Profiling build: compiles with profiling flags enabled.
        profile = pkgs.runCommand "una-profile" {
          nativeBuildInputs = [
            (pkgs.haskellPackages.ghcWithPackages (hp: [
              hp.cmdargs hp.io-storage hp.directory hp.process
              hp.filepath hp.bytestring
            ]))
          ];
        } ''
          mkdir -p $out/bin

          cp ${self}/Main.hs .

          ghc -prof -fprof-auto -threaded -rtsopts -o $out/bin/una-prof Main.hs \
            || echo "Profiling build requires profiling libraries"

          echo "Profiling binary at $out/bin/una-prof"
          echo "Run with: una-prof +RTS -p -RTS <archive>"
        '';
      };

      # ---------------------------------------------------------------
      # Apps (runnable targets)
      # ---------------------------------------------------------------

      apps = {
        default = {
          type = "app";
          program = "${unaExe}/bin/una";
        };

        # nix run .#format -- runs fourmolu in-place on all Haskell files
        format = {
          type = "app";
          program = toString (pkgs.writeShellScript "una-format" ''
            if [ $# -eq 0 ]; then
              echo "Formatting Main.hs..."
              ${fourmolu}/bin/fourmolu --mode inplace Main.hs
            else
              ${fourmolu}/bin/fourmolu --mode inplace "$@"
            fi
          '');
        };
      };

      # ---------------------------------------------------------------
      # Checks (all must pass for `nix flake check`)
      # ---------------------------------------------------------------

      checks = {

        # 1. Build check: the default package builds successfully
        build = unaExe;

        # 2. hlint: linting passes with no suggestions
        hlint = pkgs.runCommand "hlint-check" {
          nativeBuildInputs = [ hlint ];
        } ''
          cd ${self}
          hlint Main.hs
          touch $out
        '';

        # 3. fourmolu: code formatting check
        fourmolu = pkgs.runCommand "fourmolu-check" {
          nativeBuildInputs = [ fourmolu ];
        } ''
          cd ${self}
          fourmolu --mode check Main.hs
          touch $out
        '';

        # 4. warnings: build with -Werror (no warnings allowed)
        warnings = let
          unaWerror = (pkgs.haskell-nix.project' {
            src = ./.;
            supportHpack = true;
            compiler-nix-name = "ghc910";
            modules = [{
              packages.una.components.exes.una.ghcOptions = [ "-Werror" ];
            }];
          }).flake {};
        in unaWerror.packages."una:exe:una";

        # 5. Integration tests: exercise the una binary on real archives
        integration-test = pkgs.runCommand "una-integration-test" {
          nativeBuildInputs = with pkgs; [
            gnutar
            gzip
            zip
            unzip
          ];
          unaBin = unaExe;
        } ''
          set -euo pipefail
          export HOME=$(mktemp -d)
          export PATH="$unaBin/bin:$PATH"

          echo "=== Integration Test Suite ==="

          # ---- Test 1: tar.gz extraction ----
          echo "--- Test 1: tar.gz ---"
          WORKDIR=$(mktemp -d)
          cd "$WORKDIR"
          echo "hello from tar.gz test" > testfile.txt
          tar czf testarchive.tar.gz testfile.txt
          rm testfile.txt

          una testarchive.tar.gz
          if [ -f testfile.txt ] && [ "$(cat testfile.txt)" = "hello from tar.gz test" ]; then
            echo "PASS: tar.gz extraction"
          else
            echo "FAIL: tar.gz extraction"
            exit 1
          fi

          # ---- Test 2: gzip extraction ----
          echo "--- Test 2: gzip ---"
          WORKDIR2=$(mktemp -d)
          cd "$WORKDIR2"
          echo "hello from gzip test" > testfile2.txt
          gzip testfile2.txt
          # gzip creates testfile2.txt.gz

          una testfile2.txt.gz
          if [ -f testfile2.txt ] && [ "$(cat testfile2.txt)" = "hello from gzip test" ]; then
            echo "PASS: gzip extraction"
          else
            echo "FAIL: gzip extraction"
            exit 1
          fi

          # ---- Test 3: zip extraction ----
          echo "--- Test 3: zip ---"
          WORKDIR3=$(mktemp -d)
          cd "$WORKDIR3"
          echo "hello from zip test" > testfile3.txt
          zip testarchive3.zip testfile3.txt
          rm testfile3.txt

          una testarchive3.zip
          if [ -f testfile3.txt ] && [ "$(cat testfile3.txt)" = "hello from zip test" ]; then
            echo "PASS: zip extraction"
          else
            echo "FAIL: zip extraction"
            exit 1
          fi

          echo "=== All integration tests passed ==="
          touch $out
        '';
      };

      # ---------------------------------------------------------------
      # Dev Shell
      # ---------------------------------------------------------------

      devShells.default = flake.devShells.default // {
        withHoogle = true;
      };
    });
}
