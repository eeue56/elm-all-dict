module EveryDict exposing
    ( EveryDict
    , empty, singleton, insert, update
    , get, remove, member
    , eq, size, isEmpty
    , filter
    , partition
    , foldl, foldr, map
    , union, intersect, diff
    , keys, values
    , toList, fromList
    )

{-| A dictionary mapping unique keys to values. This dictionary can use any type as a key.
In the core-provided Dict, keys can only be comparable.

It uses a custom toString as a hashing function - so don't use it on any types which have non-unique
representations in strings!

It's hard to imagine many cases where you would want a function as a key, but it's now possible.

Insert, remove, and query operations all take *O(log n)* time. EveryDict
equality with `(==)` is unreliable and should not be used.

# Types
@docs EveryDict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size, eq

# Combine
@docs union, intersect, diff

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

-}

-- This is a hack
-- As EveryDict uses types with the same names as those used
-- internally by Elm-Core's Dict, when the runtime tries to
-- shortcut and pretty-print the Dict (as the ctor of the
-- objects match), it will cause an error if Dict has not been
-- imported at least somewhere in the program.
-- So, we import it here - and get prettyprinting for free!
import Dict as DeadDict

import Basics exposing (..)

-- uncomment this if you want to use native toString instead for storing functions as keys
--import TrueToString
import Maybe exposing (..)
import List exposing (..)
import Debug
import String


-- BBlack and NBlack should only be used during the deletion
-- algorithm. Any other occurrence is a bug and should fail an assert.
type NColor
    = Red
    | Black
    | BBlack  -- Double Black, counts as 2 blacks for the invariant
    | NBlack  -- Negative Black, counts as -1 blacks for the invariant


showNColor : NColor -> String
showNColor c =
  case c of
    Red    -> "Red"
    Black  -> "Black"
    BBlack -> "BBlack"
    NBlack -> "NBlack"


type LeafColor
    = LBlack
    | LBBlack -- Double Black, counts as 2


showLColor : LeafColor -> String
showLColor color =
    case color of
      LBlack  -> "LBlack"
      LBBlack -> "LBBlack"

{-| A dict which works with any type which lifts the type from Core's Dict-}
type EveryDict k v
    = RBNode_elm_builtin NColor k v (EveryDict k v) (EveryDict k v)
    | RBEmpty_elm_builtin LeafColor

ord : a -> String
ord = toString


{-| Create an empty dictionary  -}
empty : EveryDict k v
empty =
    RBEmpty_elm_builtin LBlack


{-| Element equality -}
eq : EveryDict k v -> EveryDict k v -> Bool
eq first second =
    (toList first) == (toList second)

min : EveryDict k v -> (k,v)
min dict =
    case dict of
      RBNode_elm_builtin _ key value (RBEmpty_elm_builtin LBlack) _ ->
          (key, value)

      RBNode_elm_builtin _ _ _ left _ ->
          min left

      RBEmpty_elm_builtin _ ->
          Debug.crash "(min Empty) is not defined"

max : EveryDict k v -> (k, v)
max dict =
    case dict of
      RBNode_elm_builtin _ key value _ (RBEmpty_elm_builtin _) ->
          (key, value)

      RBNode_elm_builtin _ _ _ _ right ->
          max right

      RBEmpty_elm_builtin _ ->
          Debug.crash "(max Empty) is not defined"

get_ : k -> EveryDict k v -> Maybe v
get_ targetKey dict =
    case dict of
      RBEmpty_elm_builtin _ ->
          Nothing

      RBNode_elm_builtin _ key value left right ->
          case compare (ord targetKey) (ord key) of
            LT -> get_ targetKey left
            EQ -> Just value
            GT -> get_ targetKey right

{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Mouse" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> EveryDict k v -> Maybe v
get targetKey dict =
    get_ targetKey dict


{-| Determine if a key is in a dictionary. -}
member : k -> EveryDict k v -> Bool
member key dict =
    case get_ key dict of
      Just _ -> True
      Nothing -> False


{-| Determine if a dictionary is empty.

    isEmpty empty == True
-}
isEmpty : EveryDict k v -> Bool
isEmpty dict =
    case dict of
      RBEmpty_elm_builtin _ -> True
      _ -> False

{-| Get the number of key-value pairs in a dict -}
size : EveryDict k v -> Int
size dict =
  sizeHelp 0 dict

sizeHelp : Int -> EveryDict k v -> Int
sizeHelp n dict =
  case dict of
    RBEmpty_elm_builtin _ ->
      n

    RBNode_elm_builtin _ _ _ left right ->
      sizeHelp (sizeHelp (n+1) right) left

ensureBlackRoot : EveryDict k v -> EveryDict k v
ensureBlackRoot dict =
    case dict of
      RBNode_elm_builtin Red key value left right ->
          RBNode_elm_builtin Black key value left right

      RBNode_elm_builtin Black _ _ _ _ ->
          dict

      RBEmpty_elm_builtin _ ->
          dict

      _ ->
          dict



{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : k -> v -> EveryDict k v -> EveryDict k v
insert key value dict =
    update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : k -> EveryDict k v -> EveryDict k v
remove key dict =
    update key (always Nothing) dict


type Flag = Insert | Remove | Same

showFlag : Flag -> String
showFlag f = case f of
  Insert -> "Insert"
  Remove -> "Remove"
  Same   -> "Same"


{-| Update the value of a dictionary for a specific key with a given function. -}
update : k -> (Maybe v -> Maybe v) -> EveryDict k v -> EveryDict k v
update k alter dict =
  let
      up dict =
          case dict of
            RBEmpty_elm_builtin _ ->
                case alter Nothing of
                  Nothing -> (Same, empty)
                  Just v  -> (Insert, RBNode_elm_builtin Red k v empty empty)

            RBNode_elm_builtin clr key value left right ->
                case compare (ord k) (ord key) of
                  EQ ->
                    case alter (Just value) of
                      Nothing -> (Remove, rem clr left right)
                      Just newValue ->
                          (Same, RBNode_elm_builtin clr key newValue left right)

                  LT ->
                    let (flag, newLeft) = up left in
                    case flag of
                      Same   -> (Same, RBNode_elm_builtin clr key value newLeft right)
                      Insert -> (Insert, balance clr key value newLeft right)
                      Remove -> (Remove, bubble clr key value newLeft right)

                  GT ->
                    let (flag, newRight) = up right in
                    case flag of
                      Same   -> (Same, RBNode_elm_builtin clr key value left newRight)
                      Insert -> (Insert, balance clr key value left newRight)
                      Remove -> (Remove, bubble clr key value left newRight)

      (flag, updatedDict) = up dict
  in
      case flag of
        Same   -> updatedDict
        Insert -> ensureBlackRoot updatedDict
        Remove -> blacken updatedDict


{-| Create a dictionary with one key-value pair. -}
singleton : k -> v -> EveryDict k v
singleton key value =
    insert key value empty


isBBlack : EveryDict k v -> Bool
isBBlack dict =
    case dict of
      RBNode_elm_builtin BBlack _ _ _ _ -> True
      RBEmpty_elm_builtin LBBlack -> True
      _ -> False


moreBlack : NColor -> NColor
moreBlack color =
    case color of
      Black  -> BBlack
      Red    -> Black
      NBlack -> Red
      BBlack -> Debug.crash "Can't make a double black node more black!"


lessBlack : NColor -> NColor
lessBlack color =
    case color of
      BBlack -> Black
      Black  -> Red
      Red    -> NBlack
      NBlack -> Debug.crash "Can't make a negative black node less black!"


lessBlackTree : EveryDict k v -> EveryDict k v
lessBlackTree dict =
    case dict of
      RBNode_elm_builtin c k v l r -> RBNode_elm_builtin (lessBlack c) k v l r
      RBEmpty_elm_builtin LBBlack -> RBEmpty_elm_builtin LBlack
      _ -> dict


reportRemBug : String -> NColor -> String -> String -> a
reportRemBug msg c lgot rgot =
  Debug.crash <|
    String.concat
    [ "Internal red-black tree invariant violated, expected "
    , msg, " and got ", showNColor c, "/", lgot, "/", rgot
    , "\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"
    ]


-- Remove the top node from the tree, may leave behind BBlacks
rem : NColor -> EveryDict k v -> EveryDict k v -> EveryDict k v
rem c l r =
    case (l, r) of
      (RBEmpty_elm_builtin _, RBEmpty_elm_builtin _) ->
          case c of
            Red   -> RBEmpty_elm_builtin LBlack
            Black -> RBEmpty_elm_builtin LBBlack

            _ ->
                Native.Debug.crash "cannot have bblack or nblack nodes at this point"

      (RBEmpty_elm_builtin cl, RBNode_elm_builtin cr k_ v_ l_ r_) ->
          case (c, cl, cr) of
            (Black, LBlack, Red) ->
                RBNode_elm_builtin Black k_ v_ l_ r_

            _ ->
                reportRemBug "Black/LBlack/Red" c (showLColor cl) (showNColor cr)

      (RBNode_elm_builtin cl k_ v_ l_ r_, RBEmpty_elm_builtin cr) ->
          case (c, cl, cr) of
            (Black, Red, LBlack) ->
                RBNode_elm_builtin Black k_ v_ l_ r_

            _ ->
                reportRemBug "Black/Red/LBlack" c (showNColor cl) (showLColor cr)

      -- l and r are both RBNode_elm_builtins
      (RBNode_elm_builtin cl kl vl ll rl, RBNode_elm_builtin cr kr vr lr rr) ->
          let l = RBNode_elm_builtin cl kl vl ll rl
              r = RBNode_elm_builtin cr kr vr lr rr
              (k, v) = max l
              l_     = remove_max cl kl vl ll rl
          in
              bubble c k v l_ r


-- Kills a BBlack or moves it upward, may leave behind NBlack
bubble : NColor -> k -> v -> EveryDict k v -> EveryDict k v -> EveryDict k v
bubble c k v l r =
    if isBBlack l || isBBlack r
        then balance (moreBlack c) k v (lessBlackTree l) (lessBlackTree r)
        else RBNode_elm_builtin c k v l r


-- Removes rightmost node, may leave root as BBlack
remove_max : NColor -> k -> v -> EveryDict k v -> EveryDict k v -> EveryDict k v
remove_max c k v l r =
    case r of
      RBEmpty_elm_builtin _ ->
          rem c l r

      RBNode_elm_builtin cr kr vr lr rr ->
          bubble c k v l (remove_max cr kr vr lr rr)


-- generalized tree balancing act
balance : NColor -> k -> v -> EveryDict k v -> EveryDict k v -> EveryDict k v
balance c k v l r =
    balance_node (RBNode_elm_builtin c k v l r)


blackish : EveryDict k v -> Bool
blackish t =
    case t of
      RBNode_elm_builtin c _ _ _ _ -> c == Black || c == BBlack
      RBEmpty_elm_builtin _      -> True


balance_node : EveryDict k v -> EveryDict k v
balance_node t =
  let assemble col xk xv yk yv zk zv a b c d =
        RBNode_elm_builtin (lessBlack col) yk yv (RBNode_elm_builtin Black xk xv a b) (RBNode_elm_builtin Black zk zv c d)
  in
   if blackish t
   then case t of
     RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red yk yv (RBNode_elm_builtin Red xk xv a b) c) d ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red xk xv a (RBNode_elm_builtin Red yk yv b c)) d ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red zk zv (RBNode_elm_builtin Red yk yv b c) d) ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red yk yv b (RBNode_elm_builtin Red zk zv c d)) ->
       assemble col xk xv yk yv zk zv a b c d

     RBNode_elm_builtin BBlack xk xv a (RBNode_elm_builtin NBlack zk zv (RBNode_elm_builtin Black yk yv b c) d) ->
       case d of
         (RBNode_elm_builtin Black _ _ _ _) ->
           RBNode_elm_builtin Black yk yv (RBNode_elm_builtin Black xk xv a b) (balance Black zk zv c (redden d))
         _ -> t

     RBNode_elm_builtin BBlack zk zv (RBNode_elm_builtin NBlack xk xv a (RBNode_elm_builtin Black yk yv b c)) d ->
       case a of
         (RBNode_elm_builtin Black _ _ _ _) ->
           RBNode_elm_builtin Black yk yv (balance Black xk xv (redden a) b) (RBNode_elm_builtin Black zk zv c d)
         _ -> t
     _ -> t
   else t


-- make the top node black
blacken : EveryDict k v -> EveryDict k v
blacken t =
    case t of
      RBEmpty_elm_builtin _ -> RBEmpty_elm_builtin LBlack
      RBNode_elm_builtin _ k v l r -> RBNode_elm_builtin Black k v l r


-- make the top node red
redden : EveryDict k v -> EveryDict k v
redden t =
    case t of
      RBEmpty_elm_builtin _ -> Debug.crash "can't make a Leaf red"
      RBNode_elm_builtin _ k v l r -> RBNode_elm_builtin Red k v l r


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> EveryDict k a -> EveryDict k b
map f dict =
    case dict of
      RBEmpty_elm_builtin clr ->
          RBEmpty_elm_builtin clr

      RBNode_elm_builtin clr key value left right ->
          RBNode_elm_builtin clr key (f key value) (map f left) (map f right)

{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key. -}
foldl : (k -> v -> b -> b) -> b -> EveryDict k v -> b
foldl f acc dict =
    case dict of
      RBEmpty_elm_builtin _ -> acc

      RBNode_elm_builtin _ key value left right ->
          foldl f (f key value (foldl f acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key. -}
foldr : (k -> v -> b -> b) -> b -> EveryDict k v -> b
foldr f acc t =
    case t of
      RBEmpty_elm_builtin _ -> acc

      RBNode_elm_builtin _ key value left right ->
          foldr f (f key value (foldr f acc right)) left


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary. -}
union : EveryDict k v -> EveryDict k v -> EveryDict k v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary. -}
intersect : EveryDict k v -> EveryDict k v -> EveryDict k v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : EveryDict k v -> EveryDict k v -> EveryDict k v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| Get all of the keys in a dictionary. -}
keys : EveryDict k v -> List k
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary. -}
values : EveryDict k v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs. -}
toList : EveryDict k v -> List (k,v)
toList dict =
    foldr (\key value list -> (key, value) :: list) [] dict


{-| Convert an association list into a dictionary.
Takes a default value, and a list of key-pair tuples
 -}
fromList : List (k, v) -> EveryDict k v
fromList assocs =
    List.foldl (\(key,value) dict -> insert key value dict) (empty) assocs


{-| Keep a key-value pair when it satisfies a predicate. -}
filter : (k -> v -> Bool) -> EveryDict k v -> EveryDict k v
filter predicate dictionary =
    let add key value dict =
            if predicate key value
                then insert key value dict
                else dict
    in
        foldl add (empty) dictionary


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> EveryDict k v -> (EveryDict k v, EveryDict k v)
partition predicate dict =
    let add key value (t1, t2) =
            if predicate key value
                then (insert key value t1, t2)
                else (t1, insert key value t2)
    in
        foldl add (empty, empty) dict
