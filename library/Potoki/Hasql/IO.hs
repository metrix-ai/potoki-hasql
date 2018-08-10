{-# LANGUAGE ScopedTypeVariables #-}
module Potoki.Hasql.IO where

import           Potoki.Hasql.Prelude
import qualified Hasql.Statement               as S
import qualified Hasql.Encoders                as E
import qualified Hasql.Decoders                as C
import qualified ByteString.TreeBuilder        as TB
import qualified Data.ByteString            as BS
import qualified Potoki.Hasql.Error.Hasql  as I
import           Potoki.Hasql.Error.Types
import qualified Hasql.Connection          as F
import qualified Hasql.Session             as G
import qualified Data.Vector               as D

declareCursor :: forall params. BS.ByteString -> BS.ByteString -> params -> E.Params params -> F.Connection -> IO (Either G.QueryError ())
declareCursor sql cursorName params encoder connection = G.run (G.statement params declareCursorStatement) connection
      where
        declareCursorStatement :: S.Statement params ()
        declareCursorStatement = S.Statement sql' encoder C.unit False
        sql' =
          TB.toByteString
            $  "DECLARE "
            <> TB.byteString cursorName
            <> " NO SCROLL CURSOR FOR "
            <> TB.byteString sql
            <> ";"

unitStatementIO :: BS.ByteString -> F.Connection -> IO ()
unitStatementIO sql' connection = G.run (G.statement () (S.Statement sql' E.unit C.unit True)) connection >> return ()

closeCursor :: BS.ByteString -> F.Connection -> IO ()
closeCursor cursorName = unitStatementIO $ "CLOSE " <> cursorName <> "; COMMIT;"

begin :: F.Connection -> IO ()
begin = unitStatementIO "BEGIN"

commit :: F.Connection -> IO ()
commit = unitStatementIO "COMMIT"

fetchFromCursor :: Foldable t => C.Result (t result)
  -> F.Connection 
  -> BS.ByteString 
  -> Int 
  -> ((t result) -> IO (Maybe (Either Error a)))
  -> IO (Maybe (Either Error a))
fetchFromCursor decoder connection cursorName batchSize onSuccess = do 
  let fetchFromCursorStatement = S.Statement sql' E.unit decoder True
      sql' =
        TB.toByteString
          $  "FETCH FORWARD "
          <> TB.asciiIntegral batchSize
          <> " FROM "
          <> TB.byteString cursorName
  errResult <- G.run (G.statement () fetchFromCursorStatement) connection
  case errResult of
    Left err -> return (Just (Left (I.sessionError err)))
    Right result -> onSuccess result
