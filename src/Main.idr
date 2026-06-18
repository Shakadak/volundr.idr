module Main

import Control.App
import Control.App.Console
import Data.List
import System
import System.Console.GetOpt

import Volundr.Cli.Config

-- `getArgs` gives us the program path as the first argument.
skimProgPath : List String -> List String
skimProgPath = drop 1

program : Has [Console, PrimIO] es => App es ()
program = do
  args <- primIO getArgs
  cfg <- primIO $ parse $ skimProgPath args
  putStrLn "verbose = \{show cfg.verbose}"


main : IO ()
main = run program
