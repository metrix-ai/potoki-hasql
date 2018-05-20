{-| Hasql errors interpretation -}
module Potoki.Hasql.Error.Hasql
where

import           Potoki.Hasql.Prelude
import           Potoki.Hasql.Error.Types
import qualified Hasql.Session            as A
import qualified Hasql.Connection         as C


connectionError :: C.ConnectionError -> Error
connectionError details =
  ConnectionError (maybe "" lenientUtf8ByteStringText details)

sessionError :: A.Error -> Error
sessionError =
  \ case
    A.ClientError details ->
      ConnectionError (maybe "" lenientUtf8ByteStringText details)
    A.ResultError details ->
      InteractionError ((stringText . show) details)

sessionErrorWithParams :: Show params => params -> A.Error -> Error
sessionErrorWithParams params =
  \ case
    A.ClientError details ->
      ConnectionError (maybe "" lenientUtf8ByteStringText details)
    A.ResultError details ->
      InteractionError ((fromString . shows details . showString ": " . shows params) "")
