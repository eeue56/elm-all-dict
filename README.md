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

actionDict2 : EveryDict Action String
actionDict2 =
    EveryDict.fromList
        [ (Run, "Run away!")
        , (Hide, "Coward!")
        , (StandStill, "Err...")]

main = show <| EveryDict.toList actionDict2
```


## EveryDict

EveryDict uses a custom implementation of `toString` in order to allow functions to be used as keys. The current implementation of `toString` doesn't actually work properly when functions are defined in the format

```javascript
var f = function() ...
```

This is down to the fact `toString` uses the `.name` of the functions - this is obviously not defined when using the variable binding to anonymous functions that is common in JS.
