{pkgs ? import ./nix/pin.nix {} }:
pkgs.mkShell {
  packages = [ pkgs.openapi-generator-cli ];
}
