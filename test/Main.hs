module Main where

  import Prelude hiding (assert)
  import Test.QuickCheck.Instances
  import Test.Tasty
  import Test.Tasty.Runners
  import Test.Tasty.HUnit
  import Test.Tasty.QuickCheck
  import qualified Test.QuickCheck as QuickCheck
  import qualified Hasql.Statement as Statement
  import qualified Hasql.Encoders as Encoders
  import qualified Hasql.Decoders as Decoders
  import qualified Hasql.Session as Session
  import qualified Potoki.IO as PIO
  import qualified Potoki.Consume as Consume
  import qualified Potoki.Produce as Produce
  import qualified Potoki.Transform as Transform
  import qualified Hasql.Connection as C
  import qualified Potoki.Hasql.Error.Types as Types
  import Potoki.Hasql.Error.Instances
  import Potoki.Hasql.Produce as PHP
  

  main =
    defaultMain tree
    where 
      tree =
        localOption (NumThreads 1) $
        testGroup "All tests"
        [
          testCase "IN simulation" $
          let
            settings = C.settings host port user password database
              where
                host = "localhost"
                port = 5432
                user = "postgres"
                password = ""
                database = "postgres"
            statement =
              Statement.Statement "select * from generate_series(1,100000) as X(a)" encoder decoder True
              where
                encoder =
                  Encoders.unit
                decoder =
                  Decoders.rowVector (Decoders.column Decoders.int4)
            produce = cursorStatement statement () 100 settings
            in do
              result <- PIO.produceAndConsume produce (right' Consume.count)
              assertEqual (show result) (Right 1000) result
              result <- PIO.produceAndConsume produce (right' (Consume.transform Transform.vector Consume.count))
              assertEqual (show result) (Right 100000) result
        ]
  