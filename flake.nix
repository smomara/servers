{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShell.${system} = pkgs.mkShell {
      buildInputs = with pkgs.haskellPackages; [
        ghc
        cabal-install
        network-simple
        aeson
        primes
        async
      ];
    };
  };
}
