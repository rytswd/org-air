{
  description = "org-air — modern org-agenda replacement + Air project viewer for Emacs (nix run app to generate the demo dataset anywhere).";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # The demo dataset generator: pure-stdlib Python, dates RELATIVE to
        # today, deterministic (seeded), spread evenly across the current
        # month ±2 months.  Writes ~570 entries across 50 .org files.
        #   nix run .#gen-demo              -> /tmp/org-air
        #   nix run .#gen-demo -- ~/my-org  -> a dir of your choice
        gen-demo = pkgs.writeShellApplication {
          name = "org-air-gen-demo";
          runtimeInputs = [ pkgs.python3 ];
          text = ''
            exec python3 ${./examples/demo/generate-large.py} "$@"
          '';
        };
      in
      {
        packages = {
          inherit gen-demo;
          default = gen-demo;
        };

        apps = {
          gen-demo = flake-utils.lib.mkApp { drv = gen-demo; };
          default = flake-utils.lib.mkApp { drv = gen-demo; };
        };

        # `direnv` drops you into this on `cd` (python3 to run the generator,
        # emacs + make to hack on and test org-air itself).
        devShells.default = pkgs.mkShell {
          packages = [ pkgs.python3 pkgs.emacs pkgs.gnumake ];
        };
      });
}
