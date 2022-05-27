{ nixpkgs ? import ../nix/pin.nix {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alertmanager-openapi, base, http-client, lens
      , lib, text, transformers, cabal-install
      }:
      mkDerivation {
        pname = "alertmanager-client";
        version = "0.1.0.0";
        src = ./.;
        libraryToolDepends = [ cabal-install ];
        libraryHaskellDepends = [
          alertmanager-openapi base http-client lens text transformers
        ];
        homepage = "https://github.com/SupercedeTech/alertmanager-client#readme";
        license = lib.licenses.bsd3;
      };

  haskellPackages = (if compiler == "default"
                       then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler}).extend(self: super: {
                        alertmanager-openapi = haskellPackages.callPackage ../alertmanager-openapi {};
                     });

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
