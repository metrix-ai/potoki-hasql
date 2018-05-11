module Potoki.Hasql.Fetch (
  transformFetch,
)where

import Potoki.Hasql.Prelude
import Hasql.Query
import Potoki.Core.Fetch as O
import Potoki.Hasql.Error as E
import qualified Potoki.Core.IO as I
import qualified Hasql.Connection as C
import qualified Hasql.Session as S


transformFetch :: Query a b -> (b -> c) -> C.Connection -> Fetch a -> Fetch (Either Error (a, c))
transformFetch query convert connection myFetch =
  let ioMaybeUri = I.fetch myFetch
   in O.ioMaybe $ do
     maybeUri <- ioMaybeUri
     case maybeUri of
       Nothing  -> return $ Nothing
       Just arg -> do
         eitherErrRes <- S.run (S.query arg query) connection
         return . Just $ bimap E.sessionError ((arg,) . convert) eitherErrRes

