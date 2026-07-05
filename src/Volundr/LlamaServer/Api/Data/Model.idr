module Volundr.LlamaServer.Api.Data.Model

import JSON.Simple

public export
data Status = Loaded | Unloaded

Show Status where
  show Loaded = "Loaded"
  show Unloaded = "Unloaded"

Interpolation Status where
  interpolate x = show x

FromJSON Status where
  fromJSON = withObject "Status" $ intoVariant <=< (`field` "value")
      where
        intoVariant = withString "Status" $ \x => case x of
          "loaded" => pure Loaded
          "unloaded" => pure Unloaded
          _ => fail "Unexpected value for status: \{x}"

public export
record Model where
  constructor MkModel
  id : String
  status : Status

export
Show Model where
  show model = "MkModel { id = \{model.id}, status = \{model.status} }"

export
FromJSON Model where
  fromJSON = do
    withObject "Model" $ \o => do
      id <- field o "id"
      status <- field o "status"
      pure $ MkModel { id = id, status = status }

wrapperParser : Parser JSON (List Model)
wrapperParser = withObject "Model list response" $ \o => do
  list <- field o "data"
  fromJSON list

export
decodeModels : String -> DecodingResult (List Model)
decodeModels = (mapFst JErr . wrapperParser) <=< JSON.Simple.FromJSON.decode
