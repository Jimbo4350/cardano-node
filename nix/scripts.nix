{ pkgs
, customConfigs ? [ pkgs.customConfig ]
}:
with pkgs.commonLib;
let
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "cardano-node";
      modules = [
        ./nixos/cardano-node-service.nix
        ({config, ...}: {
          services.cardano-node = let cfg = config.services.cardano-node; in {
            hostAddr = mkDefault "0.0.0.0";
            environment = mkDefault envConfig.name;
            nodeConfig = cfg.environments.${cfg.environment}.nodeConfig;
            cardanoNodePkgs = mkDefault pkgs;
            stateDir = mkDefault "state-node-${cfg.environment}";
            runtimeDir = mkDefault null;
          } // optionalAttrs (envConfig ? topology) {
            topology = mkDefault envConfig.topology;
          };
        })
      ];
    };

  in pkgs.writeScriptBin "cardano-node-${service.environment}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    mkdir -p "$(dirname "${service.socketPath}")"
    ${service.script} $@
  '';

  debugDeps = with pkgs; [
    coreutils
    findutils
    gnugrep
    gnused
    postgresql
    strace
    lsof
    dnsutils
    bashInteractive
    iproute
    curl
    netcat
    bat
    tree
  ];

in forEnvironments (environment: recurseIntoAttrs rec {
  node = mkScript environment;
  node-debug = pkgs.symlinkJoin {
    inherit (node) name;
    paths = [ node ] ++ debugDeps;
  };
})
