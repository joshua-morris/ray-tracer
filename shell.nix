{ pkgs ? import <nixpkgs> {}}:

let
  rays = pkgs.haskellPackages.callCabal2nix "rays" (gitignore ./.) {};
  gitignore = dir: pkgs.nix-gitignore.gitignoreSource [] dir;
in
  rays.env
