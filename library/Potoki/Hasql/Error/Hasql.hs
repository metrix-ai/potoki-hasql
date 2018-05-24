{-| Hasql errors interpretation -}
module Potoki.Hasql.Error.Hasql
where

import Potoki.Hasql.Prelude
import Potoki.Hasql.Error.Types
import qualified Hasql.Session as A
import qualified Hasql.Connection as C
import qualified Data.Text as B


connectionError :: C.ConnectionError -> Error
connectionError details =
  ConnectionError (maybe "" lenientUtf8ByteStringText details)

sessionError :: A.QueryError -> Error
sessionError (A.QueryError sql params commandError) =
  case commandError of
    A.ClientError details ->
      ConnectionError (maybe queryAndParamsRendering (flip mappend ("\n" <> queryAndParamsRendering) . lenientUtf8ByteStringText) details)
    A.ResultError details ->
      InteractionError (fromString (show details) <> "\n" <> queryAndParamsRendering)
  where
    queryAndParamsRendering =
      "Query: " <> lenientUtf8ByteStringText sql <> "\n" <>
      "Params: " <> B.intercalate ", " params
