{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  syslog = (
    with rec {
      syslogSource = pkgs.lib.cleanSource ../.;
      syslogBasic  = self.callCabal2nix "syslog" syslogSource { };
    };
    overrideCabal syslogBasic (old: {
    })
  );
}
