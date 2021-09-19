with (import <nixpkgs> { });
let
  # This is the last commit before ghc844 was removed.
  pkgs_ghc844 = import (builtins.fetchTarball {
    name = "nixpkgs-unstable-ghc844";
    url = "https://github.com/nixos/nixpkgs/archive/8ffedd83693d6effd1c271f3ad17c38f7dcecf42.tar.gz";
    sha256 = "1c2m2b0dslqkcwhg1yfh1mhfkc83xs1j78w9m4a2ymgcp370srs2";
  }) {};
in
haskell.lib.buildStackProject {
  name = "canvase";
  buildInputs = with pkgs_ghc844; [ pkgconfig SDL SDL_gfx cairo ];
  ghc = pkgs_ghc844.haskell.compiler.ghc844;
  src = ./.;
}
