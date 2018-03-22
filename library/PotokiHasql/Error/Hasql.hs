{-| Hasql errors interpretation -}
module PotokiHasql.Error.Hasql
where

import           PotokiHasql.Prelude
import           PotokiHasql.Error.Types
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
