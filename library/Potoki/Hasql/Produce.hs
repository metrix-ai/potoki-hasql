module Potoki.Hasql.Produce (
  vectorStatefulSession,
  statefulSession,
  cursorStatement
) where

import           Potoki.Hasql.Prelude
import qualified Data.Vector               as D
import qualified Hasql.Connection          as F
import qualified Hasql.Session             as G
import qualified Potoki.Core.Fetch         as A
import           Potoki.Core.Produce
import qualified Potoki.Core.Transform     as J
import qualified Potoki.Hasql.Error.Hasql  as I
import qualified Hasql.Statement               as S
import           Potoki.Hasql.Error.Types
import qualified Acquire.Acquire           as B
import qualified System.UUID.V4            as UUID
import           Potoki.Hasql.IO


vectorStatefulSession :: (state -> G.Session (Vector a, state)) -> state -> F.Settings -> Int -> Produce (Either Error a)
vectorStatefulSession vectorSession initialState connectionConfig buffering =
  transform
    (right' (J.takeWhile (not . D.null) >>> J.vector >>> J.bufferize buffering))
    (statefulSession vectorSession initialState connectionConfig)

statefulSession :: (state -> G.Session (a, state)) -> state -> F.Settings -> Produce (Either Error a)
statefulSession session initialState =
  havingConnectionProduce $ \ connection -> do
    stateRef <- newIORef initialState
    return $ A.Fetch $ do
      state <- readIORef stateRef
      sessionResult <- G.run (session state) connection
      case sessionResult of
        Left err -> return (Just (Left (I.sessionError err)))
        Right (result, newState) -> do
          writeIORef stateRef newState
          return (Just (Right result))

havingConnectionProduce :: (F.Connection -> IO (A.Fetch (Either Error a))) -> F.Settings -> Produce (Either Error a)
havingConnectionProduce cont connectionSettings =
  Produce $ B.Acquire $ havingConnection connectionSettings (fmap (, return ()) <$> cont)

havingConnection :: F.Settings -> (F.Connection -> IO (A.Fetch (Either Error a), IO ())) -> IO (A.Fetch (Either Error a), IO ())
havingConnection connectionSettings cont = do
    errorOrConnection <- F.acquire connectionSettings
    case errorOrConnection of
      Left err ->
        let
          fetch =
            A.Fetch $ return (Just (Left (I.connectionError err)))
          in return (fetch, return ())
      Right connection -> do
        (fetch, kill) <- cont connection
        return (fetch, kill >> F.release connection)

cursorStatement :: S.Statement params (Vector result) -> params -> Int -> F.Settings -> Produce (Either Error (Vector result))
cursorStatement (S.Statement sql encoder decoder _) params batchSize settings = 
  Produce $ B.Acquire $ do
    havingConnection settings $ \connection -> do
      randomName <- UUID.uuid
      let cursorName = fromString $ "Potoki_Hasql_" ++ filter (/= '-') (show randomName)
      begin connection >> declareCursor sql cursorName params encoder connection >>= \case
        Left err ->
          let
            fetch =
              A.Fetch $ return (Just (Left (I.sessionError err)))
            in return (fetch, return ())
        Right cursor -> do
          let kill = closeCursor cursorName connection >> commit connection 
              fetchBatch = do
                fetchFromCursor decoder connection cursorName batchSize $ \newBatch -> do
                  if null newBatch 
                    then return Nothing
                    else return $ Just $ Right newBatch
          return $ (A.Fetch fetchBatch, kill)
