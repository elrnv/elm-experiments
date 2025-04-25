module TreeUtils exposing (reinsert)
{-| This Module contains all utility functions we might need that are missing
from the tree library
-}

import MultiwayTree as Tree exposing (..)
import ListUtils exposing (..)

-- insert element from one place to the destination place in a list
reinsert : (b -> a) -> a -> a -> Tree b -> Tree b
reinsert id from to tree =
  if from == to then
    tree
  else
    ListUtils.reinsert id from to (children tree)
