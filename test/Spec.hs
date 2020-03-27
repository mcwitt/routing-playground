{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Reader
import           Test.Hspec

import           Database.PostgreSQL.Simple
import           Routing.Osm                    ( queryEdges )

connectTestDb :: IO Connection
connectTestDb = connectPostgreSQL
  "postgresql:///pgsnapshot?host=/Users/matt/src/routing-playground/postgres"

withTestDb :: ReaderT Connection IO a -> IO a
withTestDb r = connectTestDb >>= \testDb -> runReaderT r testDb

main :: IO ()
main = hspec $ do
  describe "OSM data" $ do
    xit "should load edges from db"
      $   withTestDb queryEdges
      >>= (`shouldSatisfy` (not . null))
