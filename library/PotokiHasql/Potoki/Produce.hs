module PotokiHasql.Potoki.Produce
where

import           PotokiHasql.Prelude
import qualified Hasql.Connection        as F
import qualified Hasql.Session           as G
import qualified Potoki.Core.Fetch       as A
import           Potoki.Core.Produce
import qualified Potoki.Produce          as K
import qualified Potoki.Transform        as J
import qualified PotokiHasql.Error.Hasql as I
import           PotokiHasql.Error.Types


vectorStatefulSession :: (state -> G.Session (Vector a, state)) -> state -> F.Settings -> Produce (Either Error a)
vectorStatefulSession vectorSession initialState connectionSettings =
  K.transform
    (right' J.vector)
    (statefulSession vectorSession initialState connectionSettings)


statefulSession :: (state -> G.Session (a, state)) -> state -> F.Settings -> Produce (Either Error a)
statefulSession session initialState =
  havingConnection $ \ connection -> do
    stateRef <- newIORef initialState
    return $ A.Fetch $ \ nil just -> do
      state <- readIORef stateRef
      sessionResult <- G.run (session state) connection
      case sessionResult of
        Left error -> return (just (Left (I.sessionError error)))
        Right (result, newState) -> do
          writeIORef stateRef newState
          return (just (Right result))


havingConnection :: (F.Connection -> IO (A.Fetch (Either Error a))) -> F.Settings -> Produce (Either Error a)
havingConnection cont connectionSettings =
  Produce $ do
    errorOrConnection <- F.acquire connectionSettings
    case errorOrConnection of
      Left error ->
        let
          fetch =
            A.Fetch $ \ stop yield -> return (yield (Left (I.connectionError error)))
          kill =
            return ()
          in return (fetch, kill)
      Right connection -> do
        fetch <- cont connection
        let
          kill = F.release connection
          in return (fetch, kill)
