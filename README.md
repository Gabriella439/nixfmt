# `nixfmt`

This is my Munihac project to write an automatic code formatter for the Nix
expression language.  It's very messy, pathologically slow on some code, and
also rejects some valid Nix expressions, but I'm sharing my latest draft for
other people to browse so they can see what a mess I made.

I'll probably revisit this later and polish it more when I have more time.  If
you feel the desire to contribute to this just ask!  The best way to jump in is
to submit a pull request or open an issue to discuss ways to contribute.

# Usage

    $ nix-shell -p stack haskell.compiler.ghc7103
    $ stack build
    $ stack exec -- nixfmt < somefile.nix
