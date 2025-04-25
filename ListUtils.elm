module ListUtils exposing (reinsert)
{-| This Module contains all utility functions we might need that are missing from the standard
-}

import List exposing (..)

-- insert element from one place to the destination place in a list
reinsert : (b -> a) -> a -> a -> List b -> List b
reinsert id from to list =
  if from == to then
    list
  else
    case list of
      [] -> []
      node :: rest ->
        if to == id node then
          pushToFront id from list
        else if from == id node then
          insertAfter id to node rest
        else
          node :: reinsert id from to rest

insertBefore : (b -> a) -> a -> b -> List b -> List b
insertBefore id targetid insertnode list =
  case list of
    [] -> []
    node :: rest ->
      if targetid == id node then
        insertnode :: list
      else
        node :: insertBefore id targetid insertnode rest


insertAfter : (b -> a) -> a -> b -> List b -> List b
insertAfter id targetid insertnode list =
  case list of
    [] -> []
    node :: rest ->
      node ::
        if targetid == id node then
          insertnode :: rest
        else
          insertAfter id targetid insertnode rest

-- push the node with the given id to the front of the list if found in the list
pushToFront : (b -> a) -> a -> List b -> List b
pushToFront = pushToFrontAcc []

-- accumulating helper for pushToFront
pushToFrontAcc : List b -> (b -> a) -> a -> List b -> List b
pushToFrontAcc seen id targetid remainder =
  case remainder of
    [] ->
      reverse seen
    node :: rest ->
      if targetid == id node then
        node :: ((reverse seen) ++ rest)
      else
        pushToFrontAcc (node::seen) id targetid rest
