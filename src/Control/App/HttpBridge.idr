module Control.App.HttpBridge

import Control.App
import Control.Monad.Error.Interface
import Network.HTTP

HasErr (HttpError e) es => MonadError (HttpError e) (App es) where
  throwError = throw
  catchError = catch

PrimIO es => HasIO (App es) where
  liftIO = primIO

public export
request : {e, es, a : _}
      -> Has [PrimIO, HasErr (HttpError e)] es
      => Bytestream a
      => HttpClient e
      -> Method
      -> URL
      -> List (String, String)
      -> a
      -> App es (HttpResponse, Stream (Of Bits8) (App es) ())
request = Network.HTTP.Client.request
