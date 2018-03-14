module PotokiHasql.Error.Types
where

import PotokiHasql.Prelude


{-| Error, which can be produced during interaction with the Requests database.
    Comes packed with the detailed description of the problems. -}
data Error =
  {-| Connection error. Can be interpreted as a signal for reconnecting. -}
  ConnectionError Text |
  {-| Some form of an interaction error.
      Can happen for many reasons related to improper suppositions about the state of the database. -}
  InteractionError Text
