module Main

import Control.App
import Control.App.Console
import Data.List
import System
import System.Console.GetOpt

import Network.HTTP
import Utils.String

import JSON.Simple

import Control.App.HttpBridge
import Volundr.Cli.Config
import Volundr.LlamaServer.Api.Data.Model

-- `getArgs` gives us the program path as the first argument.
skimProgPath : List String -> List String
skimProgPath = drop 1

welcome : Has [Console] es => App es ()
welcome = putStrLn "Starting Volundr (program)"

getConfig : Has [PrimIO] es => App es Config
getConfig = primIO $ parse . skimProgPath =<< getArgs

showConfig : Has [Console] es => Config -> App es ()
showConfig cfg =
  putStrLn
    """
    verbose = \{show cfg.verbose}
    server = \{cfg.server}
    message = \{cfg.message}
    """

parseServerUrl : Has [HasErr String] es => Config -> App es URL
parseServerUrl cfg =
  case url_from_string cfg.server of
    Left err => throw "Url error: \{err}"
    Right url => pure url

requestUrl : {es : _} -> Has [HasErr (HttpError String), PrimIO] es => HttpClient String -> URL -> App es (HttpResponse, Stream (Of Bits8) (App es) ())
requestUrl client url = Control.App.HttpBridge.request {e = String} client GET url [] ()

printResponse : Has [Console] es => HttpResponse -> App es ()
printResponse response = printLn response

bodyToString : Has [HasErr String] es => Stream (Of Bits8) (App es) () -> App es String
bodyToString body = do
  Just content <- utf8_pack <$> toList_ body
    | Nothing => throw "Body error: Couldn't pack the content."
  pure content

decodeBody : String -> DecodingResult (List Model)
decodeBody content = decodeModels content

getAndShowConfig : Has [Console, PrimIO] es => App es Config
getAndShowConfig = do
  cfg <- getConfig
  showConfig cfg
  pure cfg

createHttpClient : Has [PrimIO] es => App es (HttpClient String)
createHttpClient = do
  primIO new_client_default

requestFromConfig : {es : _} -> Has [HasErr String, HasErr (HttpError String), PrimIO] es => Config -> App es (Stream (Of Bits8) (App es) ())
requestFromConfig cfg = do
  url <- parseServerUrl cfg
  client <- createHttpClient
  (response, body) <- requestUrl client url
  printResponse response
  pure body

decodeAndShowBody : Has [Console] es => String -> App es ()
decodeAndShowBody content =
  printLn $ decodeBody content

program : {es : _} -> Has [Console, PrimIO, HasErr (HttpError String), HasErr String] es => App es ()
program = do
  welcome
  cfg <- getAndShowConfig
  body <- requestFromConfig cfg
  content <- bodyToString body
  putStrLn $ content
  decodeAndShowBody content

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
