{pkgs}:
rec { a = [ 1 2 3 ]; b = { b1 = true; b2 = { b21 = 1; really-really-long-name = { b221 = "foo"; b22 = [ 1 2 3 4 5 6 7 8]; }; }; };
  c = f {
    inherit a; str = ''#! ${pkgs.stdenv.shell}\n echo "Hello!"\n'';
    environment.systemPackages = with pkgs; [ tmux vim ];
  };
    d = let tool = "nixfmt"; in "${tool} is awesome";
e = let lots = "lots";
             of = "of";
                 binds = "binds";
                    which = "which";
                        were = "were";
                            originally = "originally";
                                unaligned = "unaligned"; in "wow!";
}
