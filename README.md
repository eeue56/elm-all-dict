# elm-all-dict

This is a dictionary based off of core's Dict implementation, except it can support any type, rather than just `comparable`s. A function must be given which will hash the key into a `comparable`.

```elm

module Main where

import Graphics.Element exposing (show)
import AllDict as Dict

type Action = Run | Hide | StandStill

-- The numbers could be anything here.
-- If they aren't unique, then the keys will be considered the "same"
-- This is usuful sometimes.
ord : Action -> Int
ord action =
    case action of
        Run -> 0
        Hide -> 1
        StandStill -> 2

actionDict : Dict.AllDict Action String Int
actionDict =
    Dict.fromList
        ord
        [ (Run, "Run away!")
        , (Hide, "Coward!")
        , (StandStill, "Err...")]

main = show <| Dict.toList actionDict


```
