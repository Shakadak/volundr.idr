module Main

import Control.App
import Control.App.Console
import Data.List
import System
import System.Console.GetOpt

import Network.HTTP
import Utils.String

import Control.App.HttpBridge
import Volundr.Cli.Config

-- `getArgs` gives us the program path as the first argument.
skimProgPath : List String -> List String
skimProgPath = drop 1

program : {es : _} -> Has [Console, PrimIO, HasErr (HttpError String), HasErr String] es => App es ()
program = do
  cfg <- primIO $ parse . skimProgPath =<< getArgs
  putStrLn
    """
    verbose = \{show cfg.verbose}
    server = \{cfg.server}
    message = \{cfg.message}
    """
  client <- primIO new_client_default
  url <- case url_from_string cfg.server of
    Left err => throw "Url error: \{err}"
    Right url => pure url
  (response, body) <- request {e = String} client GET url [] ()
  putStrLn $ show response
  Just content <- utf8_pack <$> toList_ body
    | Nothing => throw "Body error: Couldn't pack the content."
  putStrLn $ content

handleFor :
     (onOk : a -> App e b)
  -> (onErr : err -> App e b)
  -> App (err :: e) a
  -> App e b
handleFor onOk onErr prog = handle prog onOk onErr

main : IO ()
main =
  run
  <| handleFor {err = HttpError String}
    pure
    (\err => primIO $ putStrLn "Http error: \{show err}")
  <| handleFor {err = String}
    pure
    (\err => primIO $ putStrLn err)
  <| program
