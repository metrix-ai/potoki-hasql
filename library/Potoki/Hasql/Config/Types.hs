module Potoki.Hasql.Config.Types
where

import Potoki.Hasql.Prelude
import qualified Hasql.Pool as B
import qualified Hasql.Connection as C


{-| Settings of a Requests DB connection pool. -}
newtype PoolConfig =
  PoolConfig B.Settings

{-| Settings of a single Requests DB connection. -}
newtype ConnectionConfig =
  ConnectionConfig C.Settings
