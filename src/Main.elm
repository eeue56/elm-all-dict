module Main where

import Graphics.Element exposing (show)
import AllDict as Dict

import EveryDict


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

actionDict' : EveryDict.EveryDict Action String
actionDict' =
    EveryDict.fromList
        [ (Run, "Run away!")
        , (Hide, "Coward!")
        , (StandStill, "Err...")]


type alias Dinosaur =
    { name : String }

dino = { name = "Hello" }

eatBones : Int -> String
eatBones x = toString x

eatDogs : Int -> String
eatDogs x = (toString x) ++ "::"


dogDict =
    EveryDict.fromList
        [ (eatDogs, Run)
        , (eatBones, Hide)]

main = show <| EveryDict.get eatBones <|  dogDict
