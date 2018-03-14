module PotokiHasql.Prelude
(
  module Exports,
  textString,
  stringText,
  showAsText,
  lenientUtf8ByteStringText,
)
where

import           Prelude                  as Exports
import           Control.Foldl            as Exports (Fold(..), FoldM(..))
import           Control.Monad            as Exports
import           Data.Profunctor          as Exports
import           Data.IORef               as Exports
import           Data.Vector              as Exports (Vector(..))
import           Data.Text                as Exports (Text(..))
import qualified Data.Text                as A
import qualified Data.Text.Encoding       as A
import qualified Data.Text.Encoding.Error as A
import qualified Data.ByteString as B

textString :: A.Text -> String
textString =
  A.unpack

stringText :: String -> A.Text
stringText =
  A.pack

showAsText :: Show a => a -> A.Text
showAsText =
  stringText . show

lenientUtf8ByteStringText :: B.ByteString -> A.Text
lenientUtf8ByteStringText =
  A.decodeUtf8With A.lenientDecode
