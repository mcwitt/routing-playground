module LibSpec
  ( spec
  )
where

import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import qualified Data.Heap                     as H
import           Test.Hspec

import           Routing.Lib

-- | Small example graph.
-- @
--     graph {
--       rankdir=LR;
--       0 -- 1 [label=4];
--       0 -- 7 [label=8];
--       1 -- 7 [label=11];
--       1 -- 2 [label=8];
--       7 -- 8 [label=7];
--       2 -- 8 [label=2];
--       2 -- 3 [label=7];
--       2 -- 5 [label=4];
--       6 -- 7 [label=1];
--       8 -- 6 [label=6];
--       3 -- 5 [label=14];
--       3 -- 4 [label=9];
--       5 -- 6 [label=2];
--       4 -- 5 [label=10];
--       {rank=same; 1, 7}
--       {rank=same; 2, 8, 6}
--       {rank=same; 3, 5}
--     }
-- @
small :: AList
small = fromEdgesSymmetric
  [ Edge 0 1 4  ()
  , Edge 0 7 8  ()
  , Edge 1 7 11 ()
  , Edge 1 2 8  ()
  , Edge 7 8 7  ()
  , Edge 2 8 2  ()
  , Edge 2 3 7  ()
  , Edge 2 5 4  ()
  , Edge 6 7 1  ()
  , Edge 8 6 6  ()
  , Edge 3 5 14 ()
  , Edge 3 4 9  ()
  , Edge 5 6 2  ()
  , Edge 4 5 10 ()
  ]


spec :: Spec
spec = do
  describe "Simple Dijkstra implementation" $ do
    it "should find shortest path in small graph"
      $          spDijkstraSimple small 0 4
      `shouldBe` Just (21, 4 :| [5, 6, 7, 0])
