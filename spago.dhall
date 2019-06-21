{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "format-nix"
, dependencies =
    [ "aff"
    , "console"
    , "debug"
    , "effect"
    , "generics-rep"
    , "motsunabe"
    , "node-fs-aff"
    , "prelude"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "app/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
