module Potoki.Hasql.Prelude
(
  module Exports,
  stringText,
  lenientUtf8ByteStringText,
)
where

import           Prelude                  as Exports
import           Control.Monad            as Exports (join)
import           Data.Profunctor          as Exports (right')
import           Data.IORef               as Exports
import           Data.Vector              as Exports (Vector(..))
import           Data.Text                as Exports (Text(..))
import           Control.Arrow            as Exports ((>>>))
import qualified Data.Text                as A
import qualified Data.Text.Encoding       as A
import qualified Data.Text.Encoding.Error as A
import qualified Data.ByteString          as B


stringText :: String -> A.Text
stringText =
  A.pack

lenientUtf8ByteStringText :: B.ByteString -> A.Text
lenientUtf8ByteStringText =
  A.decodeUtf8With A.lenientDecode
