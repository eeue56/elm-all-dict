module Main where

import Graphics.Element exposing (show)
import AllDict
import Dict

import EveryDict


type Action = Run | Hide | StandStill

ord : Action -> Int
ord action =
    case action of
        Run -> 0
        Hide -> 1
        StandStill -> 2

actionDict : AllDict.AllDict Action String Int
actionDict =
    AllDict.fromList
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

durp x = toString x

eatDogs : Int -> String
eatDogs x = (toString x) ++ "::"

banana =
    Dict.fromList
        [ ("k", "v") ]

orange =
    Dict.fromList
        [ ("l", "m") ]

dogDict =
    EveryDict.fromList
        [ (banana, Run)
        , (orange, StandStill) ]

main = show <| EveryDict.get banana <|  dogDict
