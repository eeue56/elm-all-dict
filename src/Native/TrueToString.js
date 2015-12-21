Elm.Native.TrueToString = {};
Elm.Native.TrueToString.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.TrueToString = localRuntime.Native.TrueToString || {};
    if (localRuntime.Native.TrueToString.values)
    {
        return localRuntime.Native.TrueToString.values;
    }

    $Native$Show = Elm.Native.Show.make(localRuntime);
    var _toString = $Native$Show.toString;


    var toString = function(v){
        if (typeof v === "function"){
            return v.toString();
        }

        return _toString(v);
    }


    return Elm.Native.TrueToString.values = {
        toString: toString
    };
};
