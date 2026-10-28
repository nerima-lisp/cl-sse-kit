{
  description = "HTTP Server-Sent Events parser, serializer, sessions, publisher, and client protocol state.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-http-message-kit = {
      url = "github:nerima-lisp/cl-http-message-kit/v2.0.1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-hpack-codec-kit.follows = "cl-hpack-codec-kit";
    };

    cl-hpack-codec-kit = {
      url = "github:nerima-lisp/cl-hpack-codec-kit/v1.1.2";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
    };

    cl-http-kit = {
      url = "github:nerima-lisp/cl-http-kit/d332c596075af799a0d8ed908293c7477cb43f70";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-http-message-kit.follows = "cl-http-message-kit";
      inputs.cl-hpack-codec-kit.follows = "cl-hpack-codec-kit";
      inputs.cl-concurrent-kit.follows = "cl-concurrent-kit";
      inputs.cl-boundary-kit.follows = "cl-boundary-kit";
      inputs.cl-date-kit.follows = "cl-date-kit";
      inputs.cl-host-kit.follows = "cl-host-kit";
      inputs.cl-codec-kit.follows = "cl-codec-kit";
      inputs.cl-deflate-kit.follows = "cl-deflate-kit";
      inputs.cl-tls-kit.follows = "cl-tls-kit";
      inputs.cl-crypto-kit.follows = "cl-crypto-kit";
      inputs.cl-quic-kit.follows = "cl-quic-kit";
    };

    cl-deflate-kit = {
      url = "github:nerima-lisp/cl-deflate-kit/main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
    };

    cl-crypto-kit = {
      url = "github:nerima-lisp/cl-crypto-kit/main";
      flake = false;
    };

    cl-tls-kit = {
      url = "github:nerima-lisp/cl-tls-kit/v0.5.2";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-crypto-kit.follows = "cl-crypto-kit";
    };

    cl-quic-kit = {
      url = "github:nerima-lisp/cl-quic-kit/v0.2.7";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-crypto-kit.follows = "cl-crypto-kit";
      inputs.cl-tls-kit.follows = "cl-tls-kit";
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
      cl-hpack-codec-kit,
      cl-http-kit,
      cl-resilience-kit,
      cl-boundary-kit,
      cl-concurrent-kit,
      cl-date-kit,
      cl-host-kit,
      cl-deflate-kit,
      cl-tls-kit,
      cl-quic-kit,
      cl-crypto-kit,
      cl-weave,
      ...
    }:
    let
      clHttpKit = cl-http-kit.outPath;
      clDeflate = cl-deflate-kit.outPath;
      clTls = cl-tls-kit.outPath;
      clQuic = cl-quic-kit.outPath;
      clCrypto = cl-crypto-kit;
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
            version = "1.0.0";
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
              cl-hpack-codec-kit.packages.${system}.default
              cl-resilience-kit.packages.${system}.default
              cl-boundary-kit.packages.${system}.default
              cl-concurrent-kit.packages.${system}.default
              cl-date-kit.packages.${system}.default
              cl-host-kit.packages.${system}.default
              pkgs.openssl
              pkgs.python3
              cl-weave.packages.${system}.default
              pkgs.sbclPackages.alexandria
              pkgs.sbclPackages.bordeaux-threads
              pkgs.sbclPackages.global-vars
              pkgs.sbclPackages.trivial-features
              pkgs.sbclPackages.trivial-garbage
              pkgs.sbcl
              pkgs.coreutils
              pkgs.perl
            ];
            shellHook = ''
              export CL_SOURCE_REGISTRY="''${CL_SOURCE_REGISTRY:-$PWD//:${cl-codec-kit.outPath}//:${cl-http-message-kit.outPath}//:${
                cl-hpack-codec-kit.packages.${system}.default
              }/share/common-lisp/source//:${cl-resilience-kit.outPath}//:${cl-boundary-kit.outPath}//:${cl-concurrent-kit.outPath}//:${cl-date-kit.outPath}//:${cl-host-kit.outPath}//:${cl-weave.outPath}//:${clHttpKit}//:${clDeflate}//:${clTls}//:${clCrypto}//:${clQuic}//:${pkgs.sbclPackages.alexandria}/:${pkgs.sbclPackages.global-vars}/:${pkgs.sbclPackages.trivial-features}/:${pkgs.sbclPackages.trivial-garbage}/:${pkgs.sbclPackages.bordeaux-threads}/}"
            '';
          };
        }
      );

      apps = forEachSystem (
        system: pkgs:
        let
          clCodec = cl-codec-kit.packages.${system}.default;
          clHttpMessage = cl-http-message-kit.packages.${system}.default;
          clHpack = cl-hpack-codec-kit.packages.${system}.default;
          clResilience = cl-resilience-kit.packages.${system}.default;
          clBoundary = cl-boundary-kit.packages.${system}.default;
          clConcurrent = cl-concurrent-kit.packages.${system}.default;
          clDate = cl-date-kit.packages.${system}.default;
          clHost = cl-host-kit.packages.${system}.default;
          clWeave = cl-weave.packages.${system}.default;
          alexandria = pkgs.sbclPackages.alexandria;
          bordeauxThreads = pkgs.sbclPackages.bordeaux-threads;
          globalVars = pkgs.sbclPackages.global-vars;
          trivialFeatures = pkgs.sbclPackages.trivial-features;
          trivialGarbage = pkgs.sbclPackages.trivial-garbage;
          asdFiles = [
            "${clCodec}/cl-codec-kit.asd"
            "${clHpack}/share/common-lisp/source/cl-hpack-codec-kit/cl-hpack-codec-kit.asd"
            "${clHttpMessage}/share/common-lisp/source/cl-http-message-kit/cl-http-message-kit.asd"
            "${clDate}/cl-date-kit.asd"
            "${clHost}/cl-host-kit.asd"
            "${clBoundary}/cl-boundary-kit.asd"
            "${clConcurrent}/cl-concurrent-kit.asd"
            "${clResilience}/cl-resilience-kit.asd"
            "${clHttpKit}/cl-http-kit.asd"
            "${clDeflate}/cl-deflate-kit.asd"
            "${clTls}/cl-tls-kit.asd"
            "${clQuic}/cl-quic-kit.asd"
            "${clCrypto}/cl-crypto-kit.asd"
          ];
          asdLoadOptions = builtins.concatStringsSep " " (map (path: "--load ${path}") asdFiles);
          test = pkgs.writeShellApplication {
            name = "cl-sse-kit-test";
            runtimeInputs = [
              pkgs.sbcl
              pkgs.coreutils
              clCodec
              clHpack
              clHttpMessage
              clResilience
              clBoundary
              clConcurrent
              clDate
              clHost
              pkgs.openssl
              pkgs.python3
              clWeave
              alexandria
              bordeauxThreads
              globalVars
              trivialFeatures
              trivialGarbage
            ];
            text = ''
              export SBCL_HOME="${pkgs.sbcl}/lib/sbcl"
              export CL_SOURCE_REGISTRY="$PWD//:${alexandria}/:${bordeauxThreads}/:${globalVars}/:${trivialFeatures}/:${trivialGarbage}/"
              timeout --signal=TERM --kill-after=15s 300s \
                cl-weave run cl-sse-kit/test \
                --reporter spec \
                --max-workers 1 \
                --test-timeout-ms 300000 \
                --fail-with-no-tests \
                ${asdLoadOptions} \
                --load "$PWD/t/load-local-system.asd" \
                "$@"
              timeout --signal=TERM --kill-after=15s 300s \
                cl-weave run cl-sse-kit/http-kit-client/test \
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
              clHpack
              clHttpMessage
              clResilience
              clBoundary
              clConcurrent
              clDate
              clHost
              clWeave
              alexandria
              bordeauxThreads
              globalVars
              trivialFeatures
              trivialGarbage
            ];
            text = ''
              export CL_SOURCE_REGISTRY="$PWD//:${alexandria}/:${bordeauxThreads}/:${globalVars}/:${trivialFeatures}/:${trivialGarbage}/"
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
    };
}
