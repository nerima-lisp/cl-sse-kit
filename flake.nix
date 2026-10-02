{
  description = "HTTP Server-Sent Events parser, serializer, sessions, publisher, and client protocol state.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-http-message-kit = {
      url = "github:nerima-lisp/cl-http-message-kit";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
      inputs.paredit-cli.follows = "paredit-cli";
    };

    cl-http-kit = {
      url = "github:nerima-lisp/cl-http-kit/land/main-catchup";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-codec-kit.follows = "cl-codec-kit";
      inputs.cl-boundary-kit.follows = "cl-boundary-kit";
      inputs.cl-concurrent-kit.follows = "cl-concurrent-kit";
      inputs.cl-crypto-kit.follows = "cl-crypto-kit";
      inputs.cl-date-kit.follows = "cl-date-kit";
      inputs.cl-deflate-kit.follows = "cl-deflate-kit";
      inputs.cl-host-kit.follows = "cl-host-kit";
      inputs.cl-observability-kit.follows = "cl-observability-kit";
      inputs.cl-tls-kit.follows = "cl-tls-kit";
      inputs.cl-weave.follows = "cl-weave";
      inputs.paredit-cli.follows = "paredit-cli";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    cl-crypto-kit = {
      url = "github:nerima-lisp/cl-crypto-kit/takeokunn-crypto-integration";
      flake = false;
    };

    cl-deflate-kit = {
      url = "github:nerima-lisp/cl-deflate-kit/takeokunn-deflate-core";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
    };

    cl-tls-kit = {
      url = "github:nerima-lisp/cl-tls-kit/takeokunn-tls13-handshake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-crypto-kit.follows = "cl-crypto-kit";
    };

    cl-observability-kit = {
      url = "github:nerima-lisp/cl-observability-kit/v0.1.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-concurrent-kit.follows = "cl-concurrent-kit";
      inputs.cl-boundary-kit.follows = "cl-boundary-kit";
      inputs.cl-date-kit.follows = "cl-date-kit";
    };

    paredit-cli = {
      url = "github:takeokunn/paredit-cli/v1.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-resilience-kit = {
      url = "github:nerima-lisp/cl-resilience-kit/v1.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-boundary-kit.follows = "cl-resilience-kit/cl-boundary-kit";
    cl-concurrent-kit.follows = "cl-resilience-kit/cl-concurrent-kit";
    cl-date-kit.follows = "cl-resilience-kit/cl-date-kit";
    cl-host-kit.follows = "cl-resilience-kit/cl-boundary-kit/cl-host-kit";

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-codec-kit,
      cl-http-message-kit,
      cl-http-kit,
      cl-crypto-kit,
      cl-deflate-kit,
      cl-tls-kit,
      cl-observability-kit,
      paredit-cli,
      treefmt-nix,
      cl-resilience-kit,
      cl-boundary-kit,
      cl-concurrent-kit,
      cl-date-kit,
      cl-host-kit,
      cl-weave,
      ...
    }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];
      forEachSystem =
        function:
        nixpkgs.lib.genAttrs systems (system: function system (import nixpkgs { inherit system; }));
    in
    {
      formatter = forEachSystem (system: pkgs: pkgs.nixfmt-tree);

      # The source tree is installed where an ASDF source registry expects to
      # find it; there is nothing to compile ahead of time.
      packages = forEachSystem (
        system: pkgs: {
          default = pkgs.stdenvNoCC.mkDerivation {
            pname = "cl-sse-kit";
            version = "1.1.0";
            src = self;
            dontBuild = true;
            installPhase = ''
              runHook preInstall
              target="$out/share/common-lisp/source/cl-sse-kit"
              mkdir -p "$target"
              cp -r cl-sse-kit.asd src t "$target"/
              runHook postInstall
            '';
            meta = {
              description = "HTTP Server-Sent Events parser, serializer, sessions, publisher, and client protocol state";
              license = pkgs.lib.licenses.mit;
            };
          };
        }
      );

      devShells = forEachSystem (
        system: pkgs: {
          default = pkgs.mkShell {
            packages = [
              cl-codec-kit.packages.${system}.default
              cl-http-message-kit.packages.${system}.default
              cl-http-kit.sourceInfo.outPath
              cl-deflate-kit.packages.${system}.default
              cl-tls-kit.packages.${system}.default
              cl-resilience-kit.packages.${system}.default
              cl-boundary-kit.packages.${system}.default
              cl-concurrent-kit.packages.${system}.default
              cl-date-kit.packages.${system}.default
              cl-host-kit.packages.${system}.default
              cl-weave.packages.${system}.default
              pkgs.sbcl
              pkgs.coreutils
              pkgs.perl
              pkgs.openssl
            ];
          };
        }
      );

      apps = forEachSystem (
        system: pkgs:
        let
          clCodec = cl-codec-kit.packages.${system}.default;
          clHttpMessage = cl-http-message-kit.packages.${system}.default;
          clHttpKit = cl-http-kit.sourceInfo.outPath;
          clCryptoKit = cl-crypto-kit;
          clDeflate = cl-deflate-kit.packages.${system}.default;
          clTls = cl-tls-kit.packages.${system}.default;
          clResilience = cl-resilience-kit.packages.${system}.default;
          clBoundary = cl-boundary-kit.packages.${system}.default;
          clConcurrent = cl-concurrent-kit.packages.${system}.default;
          clDate = cl-date-kit.packages.${system}.default;
          clHost = cl-host-kit.packages.${system}.default;
          clWeave = cl-weave.packages.${system}.default;
          asdFiles = [
            "${clCodec}/cl-codec-kit.asd"
            "${clHttpMessage}/share/common-lisp/source/cl-http-message-kit/cl-http-message-kit.asd"
            "${clCryptoKit}/cl-crypto-kit.asd"
            "${clDeflate}/share/common-lisp/source/cl-deflate-kit/cl-deflate-kit.asd"
            "${clTls}/share/common-lisp/source/cl-tls-kit/cl-tls-kit.asd"
            "${clHttpKit}/cl-http-kit.asd"
            "${clDate}/cl-date-kit.asd"
            "${clHost}/cl-host-kit.asd"
            "${clBoundary}/cl-boundary-kit.asd"
            "${clConcurrent}/cl-concurrent-kit.asd"
            "${clResilience}/cl-resilience-kit.asd"
          ];
          asdLoadOptions = builtins.concatStringsSep " " (map (path: "--load ${path}") asdFiles);
          test = pkgs.writeShellApplication {
            name = "cl-sse-kit-test";
            runtimeInputs = [
              pkgs.sbcl
              pkgs.coreutils
              clCodec
              clHttpMessage
              clHttpKit
              clDeflate
              clTls
              clResilience
              clBoundary
              clConcurrent
              clDate
              clHost
              clWeave
            ];
            text = ''
              export CL_SOURCE_REGISTRY='(:source-registry :ignore-inherited-configuration)'
              timeout --signal=TERM --kill-after=15s 300s \
                cl-weave run cl-sse-kit/test \
                --reporter spec \
                --max-workers 1 \
                --test-timeout-ms 300000 \
                --fail-with-no-tests \
                ${asdLoadOptions} \
                --load "$PWD/t/load-local-system.asd" \
                "$@"
            '';
          };
          coverage = pkgs.writeShellApplication {
            name = "cl-sse-kit-coverage";
            runtimeInputs = [
              pkgs.sbcl
              pkgs.coreutils
              clCodec
              clHttpKit
              clDeflate
              clTls
              clResilience
              clBoundary
              clConcurrent
              clDate
              clHost
              clWeave
            ];
            text = ''
              export CL_SOURCE_REGISTRY='(:source-registry :ignore-inherited-configuration)'
              coverage_dir="$(mktemp -d "''${TMPDIR:-/tmp}/cl-sse-kit-coverage.XXXXXX")"
              trap 'rm -rf "$coverage_dir"' EXIT
              timeout --signal=TERM --kill-after=15s 300s \
                cl-weave run cl-sse-kit/test \
                --reporter spec \
                --max-workers 1 \
                --test-timeout-ms 300000 \
                --coverage \
                --coverage-system cl-sse-kit \
                --coverage-include "$PWD/src" \
                --coverage-output "$coverage_dir/coverage" \
                --coverage-report-directory "$coverage_dir/report" \
                --coverage-min-expression 100 \
                --coverage-min-branch 100 \
                ${asdLoadOptions} \
                --load "$PWD/t/load-local-system.asd" \
                "$@"
            '';
          };
        in
        {
          default = {
            type = "app";
            program = "${test}/bin/cl-sse-kit-test";
          };
          test = {
            type = "app";
            program = "${test}/bin/cl-sse-kit-test";
          };
          coverage = {
            type = "app";
            program = "${coverage}/bin/cl-sse-kit-coverage";
          };
        }
      );

      checks = forEachSystem (
        system: pkgs:
        let
          clCodec = cl-codec-kit.packages.${system}.default;
          clHttpMessage = cl-http-message-kit.packages.${system}.default;
          clHttpKit = cl-http-kit.sourceInfo.outPath;
          clCryptoKit = cl-crypto-kit;
          clDeflate = cl-deflate-kit.packages.${system}.default;
          clTls = cl-tls-kit.packages.${system}.default;
          clResilience = cl-resilience-kit.packages.${system}.default;
          clBoundary = cl-boundary-kit.packages.${system}.default;
          clConcurrent = cl-concurrent-kit.packages.${system}.default;
          clDate = cl-date-kit.packages.${system}.default;
          clHost = cl-host-kit.packages.${system}.default;
          clWeave = cl-weave.packages.${system}.default;
          asdFiles = [
            "${clCodec}/cl-codec-kit.asd"
            "${clHttpMessage}/share/common-lisp/source/cl-http-message-kit/cl-http-message-kit.asd"
            "${clCryptoKit}/cl-crypto-kit.asd"
            "${clDeflate}/share/common-lisp/source/cl-deflate-kit/cl-deflate-kit.asd"
            "${clTls}/share/common-lisp/source/cl-tls-kit/cl-tls-kit.asd"
            "${clHttpKit}/cl-http-kit.asd"
            "${clDate}/cl-date-kit.asd"
            "${clHost}/cl-host-kit.asd"
            "${clBoundary}/cl-boundary-kit.asd"
            "${clConcurrent}/cl-concurrent-kit.asd"
            "${clResilience}/cl-resilience-kit.asd"
          ];
          asdLoadOptions = builtins.concatStringsSep " " (map (path: "--load ${path}") asdFiles);
        in
        {
          test =
            pkgs.runCommand "cl-sse-kit-test"
              {
                nativeBuildInputs = [
                  pkgs.sbcl
                  pkgs.coreutils
                  pkgs.openssl
                  clWeave
                  clCodec
                  clHttpMessage
                  clHttpKit
                  clDeflate
                  clTls
                  clResilience
                  clBoundary
                  clConcurrent
                  clDate
                  clHost
                ];
              }
              ''
                cp -r ${self} source
                cd source
                export HOME="$TMPDIR/cl-sse-kit-home"
                mkdir -p "$HOME"
                export SBCL_HOME="${pkgs.sbcl}/lib/sbcl"
                export CL_SOURCE_REGISTRY='(:source-registry :ignore-inherited-configuration)'
                cl-weave run cl-sse-kit/test \
                  --reporter spec \
                  --max-workers 1 \
                  --test-timeout-ms 300000 \
                  --fail-with-no-tests \
                  ${asdLoadOptions} \
                  --load "$PWD/t/load-local-system.asd"
                touch "$out"
              '';
        }
      );
    };
}
