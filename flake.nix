{
  description = "Protocol servers";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    hsPkgs = pkgs.haskellPackages;
  in {
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        (hsPkgs.ghcWithPackages (ps: with ps; [
          # Explicit dependencies from your cabal file
          network-simple
          aeson
          primes
          async
          stm
          containers
          binary
          text
          # Development tools
          cabal-install
          haskell-language-server
        ]))
      ];
    };
  };
}
