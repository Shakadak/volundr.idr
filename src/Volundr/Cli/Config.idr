module Volundr.Cli.Config

import System.Console.GetOpt

public export
record Config where
  constructor MkConfig
  server : String
  message : String
  verbose : Bool

defaultConfig : Config
defaultConfig = MkConfig
  { server = ""
  , message = ""
  , verbose = False
  }

options : HasIO io => List (OptDescr (Config -> io Config))
options =
  [ MkOpt ['v'] ["verbose"] (NoArg (pure . { verbose := True })) "Enable verbose output."
  ]

public export
merge : HasIO io => List (Config -> io Config) -> io Config
merge = foldl (>>=) (pure defaultConfig)

public export
parse : HasIO io => List String -> io Config
parse = merge . options . getOpt RequireOrder options

