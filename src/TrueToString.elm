module TrueToString exposing (toString)

import Native.TrueToString

toString : a -> String
toString = Native.TrueToString.toString
