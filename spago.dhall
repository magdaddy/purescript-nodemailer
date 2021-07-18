{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "nodemailer"
, dependencies =
  [ "aff"
  , "console"
  , "convertable-options"
  , "effect"
  , "node-fs"
  , "node-streams"
  , "psci-support"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
