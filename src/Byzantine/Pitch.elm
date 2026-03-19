module Byzantine.Pitch exposing
    ( Pitch
    , natural, inflected, from, applyAccidental
    , PitchString, encode, decode
    , unwrapDegree, unwrapAccidental
    , isInflected, toString, position, compare
    )

{-| Pitch representation and operations.

In actual practice, this will need to be based on the tetrachord for a given
mode, not a fixed position based merely on the scale. So there will be
additional complexity that we'll need to model somehow. The same with with
attractions and inflections.


# Pitch

@docs Pitch


## Construct

@docs natural, inflected, from, applyAccidental


## Encode

@docs PitchString, encode, decode


## Unwrap

@docs unwrapDegree, unwrapAccidental


## Misc

@docs isInflected, toString, position, compare

-}

import Byzantine.Accidental as Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.PitchPosition as PitchPosition exposing (PitchPosition)
import Byzantine.Scale as Scale exposing (Scale)
import Tuple.Trio as Trio


type Pitch
    = Natural Degree
    | Inflected Accidental Degree



-- ENCODE


{-| String-encoded representation of a pitch within a scale.
-}
type alias PitchString =
    String


{-| Encode both a scale and pitch into a combined string representation. This
allows decoding without a separate scale argument.
-}
encode : Scale -> Pitch -> PitchString
encode scale pitch =
    (case pitch of
        Natural degree ->
            [ "n", Degree.toString degree ]

        Inflected accidental degree ->
            [ "i", Degree.toString degree, Accidental.toString accidental ]
    )
        |> (::) (Scale.encode scale)
        |> String.join "|"


{-| Decode a string representation that contains both scale and pitch
information. The string must be in the format:

  - "<scale>|n|<degree>" for natural pitches

  - "<scale>|i|<degree>|<accidental>" for inflected pitches

where <scale> is one of the codes defined in Scale.encode.

-}
decode : PitchString -> Result String ( Scale, Pitch )
decode pitchString =
    case String.split "|" pitchString of
        [ scaleCode, "n", degreeStr ] ->
            Result.map2 Tuple.pair
                (Scale.decode scaleCode)
                (Degree.fromString degreeStr
                    |> Result.map Natural
                )

        [ scaleCode, "i", degreeStr, accidentalStr ] ->
            Result.map3 Trio.intrine
                (Scale.decode scaleCode)
                (Degree.fromString degreeStr)
                (Accidental.fromString accidentalStr)
                |> Result.andThen
                    (\( scale, degree, accidental ) ->
                        inflected scale accidental degree
                            |> Result.map (Tuple.pair scale)
                    )

        _ ->
            Err ("Invalid format for scale and pitch: " ++ pitchString)



-- CONSTRUCT


{-| Construct a `Pitch`, which _may_ be inflected. If the degree cannot support
the accidental within the scale, this will default to a natural pitch.
-}
from : Scale -> Maybe Accidental -> Degree -> Pitch
from scale maybeAccidental degree =
    case maybeAccidental of
        Just accidental ->
            if PitchPosition.isValidInflection scale accidental degree then
                Inflected accidental degree

            else
                Natural degree

        Nothing ->
            Natural degree


{-| Wrap a degree as a `Natural` pitch.
-}
natural : Degree -> Pitch
natural degree =
    Natural degree


{-| Wrap a degree as an `Inflected` pitch.
-}
inflected : Scale -> Accidental -> Degree -> Result String Pitch
inflected scale accidental degree =
    if PitchPosition.isValidInflection scale accidental degree then
        Ok (Inflected accidental degree)

    else
        Err
            (Scale.name scale
                ++ " "
                ++ Degree.toString degree
                ++ " cannot support a "
                ++ Accidental.toString accidental
                ++ " accidental."
            )


{-| Apply the accidental to the given pitch. This will evaluate to Natural
if the accidental is not valid for that degree within the given scale, or if
it is `Nothing`.
-}
applyAccidental : Scale -> Pitch -> Maybe Accidental -> Pitch
applyAccidental scale pitch maybeAccidental =
    let
        degree =
            unwrapDegree pitch
    in
    case maybeAccidental of
        Nothing ->
            Natural degree

        Just accidental ->
            if PitchPosition.isValidInflection scale accidental degree then
                Inflected accidental degree

            else
                Natural degree



-- UNWRAP


unwrapDegree : Pitch -> Degree
unwrapDegree pitch =
    case pitch of
        Natural degree ->
            degree

        Inflected _ degree ->
            degree


unwrapAccidental : Pitch -> Maybe Accidental
unwrapAccidental pitch =
    case pitch of
        Natural _ ->
            Nothing

        Inflected accidental _ ->
            Just accidental



-- MISC


isInflected : Pitch -> Bool
isInflected pitch =
    case pitch of
        Natural _ ->
            False

        Inflected _ _ ->
            True


toString : Pitch -> String
toString pitch =
    case pitch of
        Natural degree ->
            Degree.toString degree

        Inflected accidental degree ->
            Degree.toString degree ++ " " ++ Accidental.toString accidental


{-| Get the pitch position in moria for the given pitch within a scale.
-}
position : Scale -> Pitch -> PitchPosition
position scale pitch =
    PitchPosition.pitchPosition scale (unwrapDegree pitch) (unwrapAccidental pitch)


{-| Compare two pitches within a scale by their pitch positions.
-}
compare : Scale -> Pitch -> Pitch -> Order
compare scale pitch1 pitch2 =
    PitchPosition.compare (position scale pitch1) (position scale pitch2)
