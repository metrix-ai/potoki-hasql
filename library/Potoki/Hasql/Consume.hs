module Potoki.Hasql.Consume
where

import           Potoki.Hasql.Prelude   
import qualified Hasql.Connection         as B
import qualified Hasql.Query              as E
import qualified Hasql.Session            as D
import qualified Potoki.Consume           as O
import           Potoki.Core.Consume
import qualified Potoki.Core.Fetch        as C
import qualified Potoki.Transform         as F
import qualified Potoki.Hasql.Error.Hasql as G
import           Potoki.Hasql.Error.Types  


executeBatchQueryConcurrently :: E.Query (Vector params) () -> Int -> Int -> B.Settings -> Consume params (Either Error ())
executeBatchQueryConcurrently query batchSize amountOfConnections settings =
  transform batchTransform (right' O.concat)
  where
    batchTransform =
      F.concurrently amountOfConnections (F.consume (executeBatchQuery query batchSize settings))

executeBatchQuery :: E.Query (Vector params) () -> Int -> B.Settings -> Consume params (Either Error ())
executeBatchQuery query batchSize settings =
  transform
    (F.consume (transform (F.take batchSize) O.vector))
    (executeQuery query settings)

executeQuery :: E.Query params () -> B.Settings -> Consume params (Either Error ())
executeQuery query =
  executeSession (\ params -> D.query params query)

executeSession :: (params -> D.Session ()) -> B.Settings -> Consume params (Either Error ())
executeSession session connectionSettings =
  Consume $ \ (C.Fetch fetchIO) -> do
    acquisitionResult <- B.acquire connectionSettings
    case acquisitionResult of
      Left error -> return (Left (G.connectionError error))
      Right connection ->
        let
          loop =
            join $
            fetchIO
              (return (Right ()))
              (\ params ->
                do
                  result <- D.run (session params) connection
                  case result of
                    Right () -> loop
                    Left error -> return (Left (G.sessionError error)))
          in loop <* B.release connection
