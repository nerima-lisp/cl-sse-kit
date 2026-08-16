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
              cl-resilience-kit.packages.${system}.default
              cl-boundary-kit.packages.${system}.default
              cl-concurrent-kit.packages.${system}.default
              cl-date-kit.packages.${system}.default
              cl-host-kit.packages.${system}.default
              cl-weave.packages.${system}.default
              pkgs.sbcl
              pkgs.coreutils
              pkgs.perl
            ];
          };
        }
      );

      apps = forEachSystem (
        system: pkgs:
        let
          clCodec = cl-codec-kit.packages.${system}.default;
          clHttpMessage = cl-http-message-kit.packages.${system}.default;
          clResilience = cl-resilience-kit.packages.${system}.default;
          clBoundary = cl-boundary-kit.packages.${system}.default;
          clConcurrent = cl-concurrent-kit.packages.${system}.default;
          clDate = cl-date-kit.packages.${system}.default;
          clHost = cl-host-kit.packages.${system}.default;
          clWeave = cl-weave.packages.${system}.default;
          asdFiles = [
            "${clCodec}/cl-codec-kit.asd"
            "${clHttpMessage}/share/common-lisp/source/cl-http-message-kit/cl-http-message-kit.asd"
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
              clHttpMessage
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
    };
}
