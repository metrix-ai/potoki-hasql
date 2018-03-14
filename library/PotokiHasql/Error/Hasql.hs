{-| Hasql errors interpretation -}
module PotokiHasql.Error.Hasql
where

import PotokiHasql.Prelude
import PotokiHasql.Error.Types
import qualified Hasql.Session as A
import qualified Hasql.Pool as B
import qualified Hasql.Connection as C
import qualified Data.Text.Encoding as D
import qualified Data.Text.Encoding.Error as E


connectionError :: C.ConnectionError -> Error
connectionError details =
  ConnectionError (maybe "" lenientUtf8ByteStringText details)

poolUsageError :: B.UsageError -> Error
poolUsageError =
  \ case
    B.ConnectionError details -> connectionError details
    B.SessionError details -> sessionError details

sessionError :: A.Error -> Error
sessionError =
  \ case
    A.ClientError details ->
      ConnectionError (maybe "" lenientUtf8ByteStringText details)
    A.ResultError details ->
      InteractionError ((stringText . show) details)
