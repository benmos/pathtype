let cfg = pkgs : {
  packageOverrides = pkgs : {

    localHaskellPackages = pkgs.haskellPackages_ghc783_profiling.override {
    #
    # Can switch between GHC Versions by choosing one of these above:
    #   haskellPackages_ghc763
    #   haskellPackages_ghc783
    #
          extension = self : super : {

	    # Disable Haddock globally because of current ('unstable')
	    # <nixpkgs> Darwin issues with old haddock being supplied
	    # by hydra:
	    #
	    # See: [https://github.com/NixOS/nixpkgs/issues/2280]
	    # See: [https://github.com/NixOS/nixpkgs/issues/2689]
	    # See: [https://github.com/NixOS/nixpkgs/issues/3129]
	    # See: [https://github.com/NixOS/nixpkgs/issues/3147]
	    #
	    cabal = super.cabal.override {
	      extension = self : super : {
	        noHaddock = true;
		hyperlinkSource = false;
	      };
	    };

          };
        };
  };
  };
in

let pkgs = import <nixpkgs> {config = cfg;}; in
let localHaskellPackages = pkgs.localHaskellPackages; in

let
  inherit (localHaskellPackages)

    # Haskell packages from <nixpkgs>
    # NB These /may/ make use of our 'localHaskellPackages'
    cabal
    cabalInstall_1_18_0_3
    time
    QuickCheck;

in cabal.mkDerivation (self: {
  pname = "pathtype";
  version = "0.5.4";
  src = ./.;

  # sha256 = "xxx";

  doHsColour = false;
  hyperlinkSource = false;

  buildDepends = [ QuickCheck time ];
  meta = {
    homepage = "http://code.haskell.org/pathtype";
    description = "Type-safe replacement for System.FilePath etc";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
