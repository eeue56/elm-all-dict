module TrueToString (toString) where

import Native.TrueToString

toString : a -> String
toString = Native.TrueToString.toString
