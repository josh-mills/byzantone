module Model.DegreeDataDict exposing (DegreeDataDict, init, get, set)

{-| A dictionary-like data structure for storing values associated with
Byzantine degrees.

@docs DegreeDataDict, init, get, set

-}

import Byzantine.Degree exposing (Degree(..))


{-| A dictionary mapping Byzantine degrees to values of type `a`. This type
ensures that every degree has an associated value, making it impossible to have
missing entries.
-}
type DegreeDataDict a
    = DegreeDataDict (Dict a)


type alias Dict a =
    { ga0 : a
    , di0 : a
    , ke0 : a
    , zo0 : a
    , ni1 : a
    , pa1 : a
    , bou1 : a
    , ga1 : a
    , di1 : a
    , ke1 : a
    , zo1 : a
    , ni2 : a
    , pa2 : a
    , bou2 : a
    , ga2 : a
    }


{-| Initialize a DegreeDataDict with a function that maps each degree to a
value.

    init (\_ -> 0) -- Creates a dict with 0 for every degree

    init Degree.toString -- Creates a dict mapping degrees to their string representations

-}
init : (Degree -> a) -> DegreeDataDict a
init f =
    DegreeDataDict
        { ga0 = f GA
        , di0 = f DI
        , ke0 = f KE
        , zo0 = f Zo
        , ni1 = f Ni
        , pa1 = f Pa
        , bou1 = f Bou
        , ga1 = f Ga
        , di1 = f Di
        , ke1 = f Ke
        , zo1 = f Zo_
        , ni2 = f Ni_
        , pa2 = f Pa_
        , bou2 = f Bou_
        , ga2 = f Ga_
        }


{-| Get the value associated with a degree.

    get Pa dict -- Gets the value for Pa

    get Ga_ dict -- Gets the value for upper Ga

-}
get : Degree -> DegreeDataDict a -> a
get degree (DegreeDataDict dict) =
    case degree of
        GA ->
            dict.ga0

        DI ->
            dict.di0

        KE ->
            dict.ke0

        Zo ->
            dict.zo0

        Ni ->
            dict.ni1

        Pa ->
            dict.pa1

        Bou ->
            dict.bou1

        Ga ->
            dict.ga1

        Di ->
            dict.di1

        Ke ->
            dict.ke1

        Zo_ ->
            dict.zo1

        Ni_ ->
            dict.ni2

        Pa_ ->
            dict.pa2

        Bou_ ->
            dict.bou2

        Ga_ ->
            dict.ga2


{-| Set the value for a specific degree.

    set Di newValue dict -- Updates the value for Di

    set Zo_ newValue dict -- Updates the value for upper Zo

-}
set : Degree -> a -> DegreeDataDict a -> DegreeDataDict a
set degree value (DegreeDataDict dict) =
    case degree of
        GA ->
            DegreeDataDict { dict | ga0 = value }

        DI ->
            DegreeDataDict { dict | di0 = value }

        KE ->
            DegreeDataDict { dict | ke0 = value }

        Zo ->
            DegreeDataDict { dict | zo0 = value }

        Ni ->
            DegreeDataDict { dict | ni1 = value }

        Pa ->
            DegreeDataDict { dict | pa1 = value }

        Bou ->
            DegreeDataDict { dict | bou1 = value }

        Ga ->
            DegreeDataDict { dict | ga1 = value }

        Di ->
            DegreeDataDict { dict | di1 = value }

        Ke ->
            DegreeDataDict { dict | ke1 = value }

        Zo_ ->
            DegreeDataDict { dict | zo1 = value }

        Ni_ ->
            DegreeDataDict { dict | ni2 = value }

        Pa_ ->
            DegreeDataDict { dict | pa2 = value }

        Bou_ ->
            DegreeDataDict { dict | bou2 = value }

        Ga_ ->
            DegreeDataDict { dict | ga2 = value }
