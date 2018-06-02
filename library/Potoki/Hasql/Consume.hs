module Potoki.Hasql.Consume
where

import Potoki.Hasql.Prelude   
import Potoki.Hasql.Error.Types  
import Potoki.Core.Consume
import qualified Hasql.Connection as B
import qualified Hasql.Statement as E
import qualified Hasql.Session as D
import qualified Potoki.Consume as O
import qualified Potoki.Core.Fetch as C
import qualified Potoki.Transform as F
import qualified Potoki.Hasql.Error.Hasql as G


executeBatchStatementConcurrently :: E.Statement (Vector params) () -> Int -> Int -> B.Settings -> Consume params (Either Error ())
executeBatchStatementConcurrently statement batchSize amountOfConnections settings =
  transform batchTransform (right' O.concat)
  where
    batchTransform =
      F.concurrently amountOfConnections (F.consume (executeBatchStatement statement batchSize settings))

executeBatchStatement :: E.Statement (Vector params) () -> Int -> B.Settings -> Consume params (Either Error ())
executeBatchStatement statement batchSize settings =
  transform
    (F.consume (transform (F.take batchSize) O.vector))
    (executeStatement statement settings)

executeStatement :: E.Statement params () -> B.Settings -> Consume params (Either Error ())
executeStatement statement =
  executeSession (\ params -> D.statement params statement)

executeSession :: (params -> D.Session ()) -> B.Settings -> Consume params (Either Error ())
executeSession session connectionSettings =
  Consume $ \ (C.Fetch fetchIO) -> do
    acquisitionResult <- B.acquire connectionSettings
    case acquisitionResult of
      Left err -> return (Left (G.connectionError err))
      Right connection ->
        let
          doLoop = do
            fetch <- fetchIO
            case fetch of
              Nothing     -> return (Right ())
              Just params -> do
                result <- D.run (session params) connection
                case result of
                  Right () -> doLoop
                  Left err -> return (Left (G.sessionError err))
          in doLoop <* B.release connection
