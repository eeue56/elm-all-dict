module AllDict
    ( AllDict
    , empty, singleton, insert, update
    , get, remove, member, getOrd
    , eq, fullEq, size, isEmpty
    , filter
    , partition
    , foldl, foldr, map
    , union, intersect, diff
    , keys, values
    , toList, fromList
    ) where

{-| A dictionary mapping unique keys to values. This dictionary can use any type as a key.
In the core-provided Dict, keys can only be comparable.

Insert, remove, and query operations all take *O(log n)* time. AllDictionary
equality with `(==)` is unreliable and should not be used.

# Types
@docs AllDict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size, getOrd, eq, fullEq

# Combine
@docs union, intersect, diff

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

-}

-- This is a hack
-- As AllDict uses types with the same names as those used
-- internally by Elm-Core's Dict, when the runtime tries to
-- shortcut and pretty-print the Dict (as the ctor of the
-- objects match), it will cause an error if Dict has not been
-- imported at least somewhere in the program.
-- So, we import it here - and get prettyprinting for free!
import Dict as DeadDict

import Basics exposing (..)
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
type AllDict k v comparable
    = RBNode NColor k v (AllDict k v comparable) (AllDict k v comparable)
    | RBEmpty LeafColor (k -> comparable)


{-| Create an empty dictionary using a given ord function to calculate hashes -}
empty : (k -> comparable) -> AllDict k v comparable
empty ord =
    RBEmpty LBlack ord


{-| Element equality. Does not check equality of base -}
eq : AllDict k v comparable -> AllDict k v comparable -> Bool
eq first second =
    (toList first) == (toList second)

{-| Base + element equality -}
fullEq : AllDict k v comparable -> AllDict k v comparable -> Bool
fullEq first second =
    (toList first == toList second) && (getOrd first == getOrd second)

min : AllDict k v comparable -> (k,v)
min dict =
    case dict of
      RBNode _ key value (RBEmpty LBlack _) _ ->
          (key, value)

      RBNode _ _ _ left _ ->
          min left

      RBEmpty LBlack v ->
          Debug.crash "(min Empty) is not defined"

max : AllDict k v comparable -> (k, v)
max dict =
    case dict of
      RBNode _ key value _ (RBEmpty _ _) ->
          (key, value)

      RBNode _ _ _ _ right ->
          max right

      RBEmpty _ _ ->
          Debug.crash "(max Empty) is not defined"

get' : k -> AllDict k v comparable -> Maybe v
get' targetKey dict =
    let
      ord = getOrd dict
    in
      case dict of
        RBEmpty LBlack _ ->
            Nothing

        RBNode _ key value left right ->
            case compare (ord targetKey) (ord key) of
              LT -> get' targetKey left
              EQ -> Just value
              GT -> get' targetKey right

{-| Helper function for grabbing the default value used in the dict
-}
getOrd : AllDict k v comparable -> (k -> comparable)
getOrd dict =
    case dict of
      RBEmpty LBlack ord ->
        ord
      RBNode _ _ _ left _ ->
        getOrd left


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Mouse" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> AllDict k v comparable -> Maybe v
get targetKey dict =
    get' targetKey dict


{-| Determine if a key is in a dictionary. -}
member : k -> AllDict k v comparable -> Bool
member key dict =
    case get' key dict of
      Just _ -> True
      Nothing -> False


{-| Determine if a dictionary is empty.

    isEmpty empty == True
-}
isEmpty : AllDict k v comparable -> Bool
isEmpty dict =
    case dict of
      RBEmpty _ _ -> True
      _ -> False

{-| Get the number of key-value pairs in a dict -}
size : AllDict k v comparable -> Int
size dict =
  sizeHelp 0 dict

sizeHelp : Int -> AllDict k v comparable -> Int
sizeHelp n dict =
  case dict of
    RBEmpty _ _ ->
      n

    RBNode _ _ _ left right ->
      sizeHelp (sizeHelp (n+1) right) left

ensureBlackRoot : AllDict k v comparable -> AllDict k v comparable
ensureBlackRoot dict =
    case dict of
      RBNode Red key value left right ->
          RBNode Black key value left right

      RBNode Black _ _ _ _ ->
          dict

      RBEmpty LBlack v ->
          dict


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : k -> v -> AllDict k v comparable -> AllDict k v comparable
insert key value dict =
    update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : k -> AllDict k v comparable -> AllDict k v comparable
remove key dict =
    update key (always Nothing) dict


type Flag = Insert | Remove | Same

showFlag : Flag -> String
showFlag f = case f of
  Insert -> "Insert"
  Remove -> "Remove"
  Same   -> "Same"


{-| Update the value of a dictionary for a specific key with a given function. -}
update : k -> (Maybe v -> Maybe v) -> AllDict k v comparable -> AllDict k v comparable
update k alter dict =
  let

      ord =
          getOrd dict
      empty' =
          empty ord
      up dict =
          case dict of
            RBEmpty LBlack _ ->
                case alter Nothing of
                  Nothing -> (Same, empty')
                  Just v  -> (Insert, RBNode Red k v empty' empty')

            RBNode clr key value left right ->
                case compare (ord k) (ord key) of
                  EQ ->
                    case alter (Just value) of
                      Nothing -> (Remove, rem clr left right)
                      Just newValue ->
                          (Same, RBNode clr key newValue left right)

                  LT ->
                    let (flag, newLeft) = up left in
                    case flag of
                      Same   -> (Same, RBNode clr key value newLeft right)
                      Insert -> (Insert, balance clr key value newLeft right)
                      Remove -> (Remove, bubble clr key value newLeft right)

                  GT ->
                    let (flag, newRight) = up right in
                    case flag of
                      Same   -> (Same, RBNode clr key value left newRight)
                      Insert -> (Insert, balance clr key value left newRight)
                      Remove -> (Remove, bubble clr key value left newRight)

      (flag, updatedDict) = up dict
  in
      case flag of
        Same   -> updatedDict
        Insert -> ensureBlackRoot updatedDict
        Remove -> blacken updatedDict


{-| Create a dictionary with one key-value pair. -}
singleton : (k -> comparable) -> k -> v -> AllDict k v comparable
singleton ord key value =
    insert key value (empty ord)


isBBlack : AllDict k v comparable -> Bool
isBBlack dict =
    case dict of
      RBNode BBlack _ _ _ _ -> True
      RBEmpty LBBlack _ -> True
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


lessBlackTree : AllDict k v comparable -> AllDict k v comparable
lessBlackTree dict =
    case dict of
      RBNode c k v l r -> RBNode (lessBlack c) k v l r
      RBEmpty LBBlack v -> RBEmpty LBlack v


reportRemBug : String -> NColor -> String -> String -> a
reportRemBug msg c lgot rgot =
  Debug.crash <|
    String.concat
    [ "Internal red-black tree invariant violated, expected "
    , msg, " and got ", showNColor c, "/", lgot, "/", rgot
    , "\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"
    ]


-- Remove the top node from the tree, may leave behind BBlacks
rem : NColor -> AllDict k v comparable -> AllDict k v comparable -> AllDict k v comparable
rem c l r =
    case (l, r) of
      (RBEmpty _ ord, RBEmpty _ _) ->
          case c of
            Red   -> RBEmpty LBlack ord
            Black -> RBEmpty LBBlack ord

      (RBEmpty cl v, RBNode cr k' v' l' r') ->
          case (c, cl, cr) of
            (Black, LBlack, Red) ->
                RBNode Black k' v' l' r'

            _ ->
                reportRemBug "Black/LBlack/Red" c (showLColor cl) (showNColor cr)

      (RBNode cl k' v' l' r', RBEmpty cr v) ->
          case (c, cl, cr) of
            (Black, Red, LBlack) ->
                RBNode Black k' v' l' r'

            _ ->
                reportRemBug "Black/Red/LBlack" c (showNColor cl) (showLColor cr)

      -- l and r are both RBNodes
      (RBNode cl kl vl ll rl, RBNode cr kr vr lr rr) ->
          let l = RBNode cl kl vl ll rl
              r = RBNode cr kr vr lr rr
              (k, v) = max l
              l'     = remove_max cl kl vl ll rl
          in
              bubble c k v l' r


-- Kills a BBlack or moves it upward, may leave behind NBlack
bubble : NColor -> k -> v -> AllDict k v comparable -> AllDict k v comparable -> AllDict k v comparable
bubble c k v l r =
    if isBBlack l || isBBlack r
        then balance (moreBlack c) k v (lessBlackTree l) (lessBlackTree r)
        else RBNode c k v l r


-- Removes rightmost node, may leave root as BBlack
remove_max : NColor -> k -> v -> AllDict k v comparable -> AllDict k v comparable -> AllDict k v comparable
remove_max c k v l r =
    case r of
      RBEmpty _ v ->
          rem c l r

      RBNode cr kr vr lr rr ->
          bubble c k v l (remove_max cr kr vr lr rr)


-- generalized tree balancing act
balance : NColor -> k -> v -> AllDict k v comparable -> AllDict k v comparable -> AllDict k v comparable
balance c k v l r =
    balance_node (RBNode c k v l r)


blackish : AllDict k v comparable -> Bool
blackish t =
    case t of
      RBNode c _ _ _ _ -> c == Black || c == BBlack
      RBEmpty _ v      -> True


balance_node : AllDict k v comparable -> AllDict k v comparable
balance_node t =
  let assemble col xk xv yk yv zk zv a b c d =
        RBNode (lessBlack col) yk yv (RBNode Black xk xv a b) (RBNode Black zk zv c d)
  in
   if blackish t
   then case t of
     RBNode col zk zv (RBNode Red yk yv (RBNode Red xk xv a b) c) d ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode col zk zv (RBNode Red xk xv a (RBNode Red yk yv b c)) d ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode col xk xv a (RBNode Red zk zv (RBNode Red yk yv b c) d) ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode col xk xv a (RBNode Red yk yv b (RBNode Red zk zv c d)) ->
       assemble col xk xv yk yv zk zv a b c d

     RBNode BBlack xk xv a (RBNode NBlack zk zv (RBNode Black yk yv b c) d) ->
       case d of
         (RBNode Black _ _ _ _) ->
           RBNode Black yk yv (RBNode Black xk xv a b) (balance Black zk zv c (redden d))
         _ -> t

     RBNode BBlack zk zv (RBNode NBlack xk xv a (RBNode Black yk yv b c)) d ->
       case a of
         (RBNode Black _ _ _ _) ->
           RBNode Black yk yv (balance Black xk xv (redden a) b) (RBNode Black zk zv c d)
         _ -> t
     _ -> t
   else t


-- make the top node black
blacken : AllDict k v comparable -> AllDict k v comparable
blacken t =
    case t of
      RBEmpty _ ord -> RBEmpty LBlack ord
      RBNode _ k v l r -> RBNode Black k v l r


-- make the top node red
redden : AllDict k v comparable -> AllDict k v comparable
redden t =
    case t of
      RBEmpty _ _ -> Debug.crash "can't make a Leaf red"
      RBNode _ k v l r -> RBNode Red k v l r


{-| Apply a function to all values in a dictionary.
Notice that this function takes a function of type `comparable -> a -> a`,
rather than Dict's `comparable a -> b`. If you want to provide a new default,
see mapWithDefault -}
map : (k -> a -> a) -> AllDict k a comparable -> AllDict k a comparable
map f dict =
    case dict of
      RBEmpty LBlack ord ->
          dict

      RBNode clr key value left right ->
          RBNode clr key (f key value) (map f left) (map f right)

{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key. -}
foldl : (k -> v -> b -> b) -> b -> AllDict k v comparable -> b
foldl f acc dict =
    case dict of
      RBEmpty LBlack v -> acc

      RBNode _ key value left right ->
          foldl f (f key value (foldl f acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key. -}
foldr : (k -> v -> b -> b) -> b -> AllDict k v comparable -> b
foldr f acc t =
    case t of
      RBEmpty LBlack v -> acc

      RBNode _ key value left right ->
          foldr f (f key value (foldr f acc right)) left


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary. -}
union : AllDict k v comparable -> AllDict k v comparable -> AllDict k v comparable
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary. -}
intersect : AllDict k v comparable -> AllDict k v comparable -> AllDict k v comparable
intersect t1 t2 =
    filter (\k _ -> k `member` t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : AllDict k v comparable -> AllDict k v comparable -> AllDict k v comparable
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| Get all of the keys in a dictionary. -}
keys : AllDict k v comparable -> List k
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary. -}
values : AllDict k v comparable -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs. -}
toList : AllDict k v comparable -> List (k,v)
toList dict =
    foldr (\key value list -> (key, value) :: list) [] dict


{-| Convert an association list into a dictionary.
Takes a default value, and a list of key-pair tuples
 -}
fromList : (k -> comparable) -> List (k, v) -> AllDict k v comparable
fromList ord assocs =
    List.foldl (\(key,value) dict -> insert key value dict) (empty ord) assocs


{-| Keep a key-value pair when it satisfies a predicate. -}
filter : (k -> v -> Bool) -> AllDict k v comparable -> AllDict k v comparable
filter predicate dictionary =
    let add key value dict =
            if predicate key value
                then insert key value dict
                else dict
    in
        foldl add (empty (getOrd dictionary)) dictionary


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> AllDict k v comparable -> (AllDict k v comparable, AllDict k v comparable)
partition predicate dict =
    let add key value (t1, t2) =
            if predicate key value
                then (insert key value t1, t2)
                else (t1, insert key value t2)
        ord =
          getOrd dict
    in
        foldl add (empty ord, empty ord) dict
