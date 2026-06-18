module Main

import Control.App
import Control.App.Console
import System

program : Has [Console, PrimIO] es => App es ()
program = do
  args <- primIO getArgs
  traverse_ putStrLn args


main : IO ()
main = run program
