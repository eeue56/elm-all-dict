# elm-all-dict

This library provides two Dict implementations. Firstly, it provides `AllDict`, a dictionary which you may pass a hashing function in order to allow all types to be stored as keys.

Secondly, it provides `EveryDict`, a dictionary which uses `toString` as a hashing function.

This is a dictionary based off of core's Dict implementation, except it can support any type, rather than just `comparable`s. A function must be given which will hash the key into a `comparable`.

```elm

module Main where

import Graphics.Element exposing (show)
import AllDict exposing (AllDict)
import EveryDict exposing (EveryDict)

type Action = Run | Hide | StandStill

ord : Action -> Int
ord action =
    case action of
        Run -> 0
        Hide -> 1
        StandStill -> 2

actionDict : AllDict Action String Int
actionDict =
    AllDict.fromList
        ord
        [ (Run, "Run away!")
        , (Hide, "Coward!")
        , (StandStill, "Err...")]

actionDict' : EveryDict Action String
actionDict' =
    EveryDict.fromList
        [ (Run, "Run away!")
        , (Hide, "Coward!")
        , (StandStill, "Err...")]

main = show <| EveryDict.toList actionDict'
```
