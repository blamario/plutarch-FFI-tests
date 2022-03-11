{
  description = "plutarch-ffi-tests";

  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.plutus.url = "github:input-output-hk/plutus";
  inputs.plutus-apps.url = "github:input-output-hk/plutus-apps";

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutus, plutus-apps }:
    let
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;
        in
        pkgs.haskell-nix.project' {
          src = ./.;
          compiler-nix-name = "ghc8107";
          projectFileName = "cabal.project";
          modules = [{
            packages = {
              plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
              plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
              # you might have to put deferPluginErrors for more packages here
            };
          }];
          shell = {
            withHoogle = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [ pkgs.cabal-install pkgs.hlint ];

            tools = {
              ghcid = {};
              haskell-language-server = {};
            };
          };
          sha256map = {
            # put your sha256 map here (or put them in your cabal.project directly)
            "https://github.com/plutonomicon/Plutarch"."de963f641d7474d14f9089bbf97cb1754c35093f"
              = "138mxkwad39jlx0rdqiasfmq0q29przdvqixwwa8m6kxjggny6si";
            "https://github.com/Liqwid-Labs/plutus-extra/"."1ccf21b88feb94696e2adb7575b84dba99c52611"
              = "0rpfjxv5n48kpwzw4qzcya6ap8wjjnfga2rdbkq5f74sqm6qa8s5";
            "https://github.com/input-output-hk/cardano-addresses"."71006f9eb956b0004022e80aadd4ad50d837b621"
              = "11dl3fmq7ry5wdmz8kw07ji8yvrxnrsf7pgilw5q9mi4aqyvnaqk";
            "https://github.com/input-output-hk/cardano-base"."41545ba3ac6b3095966316a99883d678b5ab8da8"
              = "0icq9y3nnl42fz536da84414av36g37894qnyw4rk3qkalksqwir";
            "https://github.com/input-output-hk/cardano-crypto"."f73079303f663e028288f9f4a9e08bcca39a923e"
              = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
            "https://github.com/input-output-hk/cardano-ledger"."1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5"
              = "0avzyiqq0m8njd41ck9kpn992yq676b1az9xs77977h7cf85y4wm";
            "https://github.com/input-output-hk/cardano-node"."814df2c146f5d56f8c35a681fe75e85b905aed5d"
              = "1hr00wqzmcyc3x0kp2hyw78rfmimf6z4zd4vv85b9zv3nqbjgrik";
            "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555"
              = "00h10l5mmiza9819p9v5q5749nb9pzgi20vpzpy1d34zmh6gf1cj";
            "https://github.com/input-output-hk/cardano-wallet"."a5085acbd2670c24251cf8d76a4e83c77a2679ba"
              = "1apzfy7qdgf6l0lb3icqz3rvaq2w3a53xq6wvhqnbfi8i7cacy03";
            "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba"
              = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
            "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c"
              = "1il8fx3misp3650ryj368b3x95ksz01zz3x0z9k00807j93d0ka0";
            "https://github.com/input-output-hk/ouroboros-network"."d2d219a86cda42787325bb8c20539a75c2667132"
            = "18xk7r0h2pxrbx76d6flsxifh0a9rz1cj1rjqs1pbs5kdmy8b7kx";
            "https://github.com/input-output-hk/plutus"."d4f933d25ecc35a9c5bb057f5cf462112129cfdb"
              = "1l6r2jlrxgswy9k59x5rapcw2035h1vs563k4vk6a9c4nhid1444";
            "https://github.com/input-output-hk/plutus-apps"."13836ecf59649ca522471417b07fb095556eb981"
              = "02hwxqvfi14x0mvg6cppyihqy9hqxq05yzlb2aglgm9fbwsd3fqs";
            "https://github.com/Quid2/flat"."ee59880f47ab835dbd73bea0847dab7869fc20d8"
              = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
            "https://github.com/input-output-hk/purescript-bridge"."47a1f11825a0f9445e0f98792f79172efef66c00"
              = "0da1vn2l6iyfxcjk58qal1l4755v92zi6yppmjmqvxf1gacyf9px";
            "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963"
              = "10pb0yfp80jhb9ryn65a4rha2lxzsn2vlhcc6xphrrkf4x5lhzqc";
            "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d"
              = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
          };
        };
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
