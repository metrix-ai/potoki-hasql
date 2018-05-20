module Potoki.Hasql.Error.Types
where

import Potoki.Hasql.Prelude


{-| Error, which can be produced during interaction with the database.
    Comes packed with a detailed description of the problems. -}
data Error =
  {-| Connection error. Can be interpreted as a signal for reconnecting. -}
  ConnectionError Text |
  {-| Some form of an interaction error.
      Can happen for many reasons related to improper suppositions about the state of the database. -}
  InteractionError Text
