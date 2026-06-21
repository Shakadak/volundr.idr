module Volundr.Cli.Config

import System
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

-- defined later
help : HasIO io => io Config

options : HasIO io => List (OptDescr (Config -> io Config))
options =
  [ MkOpt ['v'] ["verbose"] (NoArg (pure . { verbose := True })) "Enable verbose output."
  , MkOpt [] ["server"] (ReqArg (\address => pure . { server := address}) "SERVER") "The server to query."
  , MkOpt [] ["message"] (ReqArg (\message => pure . { message := message}) "MESSAGE") "The server to query."
  , MkOpt ['h'] ["help"] (NoArg $ const help) "Show this help."
  ]

help = do
  putStrLn $ usageInfo "Usage: volundr [OPTIONS]" $ options {io = io}
  exitWith ExitSuccess

merge : HasIO io => List (Config -> io Config) -> io Config
merge = foldl (>>=) (pure defaultConfig)

public export
parse : HasIO io => List String -> io Config
parse = merge . options . getOpt RequireOrder options

