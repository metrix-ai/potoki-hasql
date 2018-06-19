module Potoki.Hasql.Produce (
  vectorStatefulSession,
  statefulSession
) where

import           Potoki.Hasql.Prelude
import qualified Data.Vector               as D
import qualified Hasql.Connection          as F
import qualified Hasql.Session             as G
import qualified Potoki.Core.Fetch         as A
import           Potoki.Core.Produce
import qualified Potoki.Core.Transform     as J
import qualified Potoki.Hasql.Error.Hasql  as I
import           Potoki.Hasql.Error.Types
import qualified Acquire.Acquire           as B


vectorStatefulSession :: (state -> G.Session (Vector a, state)) -> state -> F.Settings -> Int -> Produce (Either Error a)
vectorStatefulSession vectorSession initialState connectionConfig buffering =
  transform
    (right' (J.takeWhile (not . D.null) >>> J.vector >>> J.bufferize buffering))
    (statefulSession vectorSession initialState connectionConfig)

statefulSession :: (state -> G.Session (a, state)) -> state -> F.Settings -> Produce (Either Error a)
statefulSession session initialState =
  havingConnection $ \ connection -> do
    stateRef <- newIORef initialState
    return $ A.Fetch $ do
      state <- readIORef stateRef
      sessionResult <- G.run (session state) connection
      case sessionResult of
        Left err -> return (Just (Left (I.sessionError err)))
        Right (result, newState) -> do
          writeIORef stateRef newState
          return (Just (Right result))

havingConnection :: (F.Connection -> IO (A.Fetch (Either Error a))) -> F.Settings -> Produce (Either Error a)
havingConnection cont connectionSettings =
  Produce $ B.Acquire $ do
    errorOrConnection <- F.acquire connectionSettings
    case errorOrConnection of
      Left err ->
        let
          fetch =
            A.Fetch $ return (Just (Left (I.connectionError err)))
          kill =
            return ()
          in return (fetch, kill)
      Right connection -> do
        fetch <- cont connection
        let kill = F.release connection
        return (fetch, kill)
