{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "arrays"
    , "bouzuya-command-line-option-parser"
    , "console"
    , "effect"
    , "node-child-process"
    , "node-fs-aff"
    , "node-process"
    , "pathy"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
