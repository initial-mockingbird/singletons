{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    ghc-wasm.url = "git+https://gitlab.haskell.org/ghc/ghc-wasm-meta"; 
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed; 
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: 
        let 
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack"; # will be available as the usual `stack` in terminal
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };
        in {
        haskellProjects.default = {
          projectFlakeName = "singletons";
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          #basePackages = pkgs.haskellPackages // inputs.ghc-wasm.packages;
          basePackages = pkgs.haskell.packages.ghc982;
          
          devShell = {
            hlsCheck.enable = true;
            hoogle = true;

           };
          autoWire = [ "packages" "apps" "checks" ];
        };
        packages.default = self'.packages.singletons-base;
        
        # haskell-flake doesn't set the default package, but you can do it here.
        #projectFlakeName = "singletons";
        
        devShells.default = pkgs.mkShell {
          name = "singletons";
          meta.description = "Haskell development environment";
          # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = 
            [ inputs.ghc-wasm.packages.${pkgs.system}.all_9_10
              stack-wrapped
              pkgs.hpack
              pkgs.just
            ];
        };
      };
    };
}
