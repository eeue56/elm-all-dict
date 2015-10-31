module Main where

import Graphics.Element exposing (show)
import AllDict as Dict

type Action = Run | Hide | StandStill

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
