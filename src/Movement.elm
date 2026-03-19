module Movement exposing (Movement(..), applyAccidental, isValid, ofInterval, unwrapTargetPitch)

{-| A representation of motion within intervallic space. This is more a
presentational concern rather than a theoretical concept within the system, so,
for example, there is no purpose in modeling the ison.
-}

import Byzantine.Accidental exposing (Accidental)
import Byzantine.Degree as Degree
import Byzantine.Interval exposing (Interval)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale exposing (Scale)


type Movement
    = AscendTo Pitch
    | DescendTo Pitch
    | None


{-| Given a starting pitch (i.e., the current pitch) and an interval
representing the step between two adjacent degrees in the scale, what is the
movement from the current pitch to the next pitch represented by that interval?

TODO: I think this needs work. This assumes that intervals are adjacent steps,
and ascending. See also the note on `isValid` below.

-}
ofInterval : Maybe Pitch -> Interval -> Movement
ofInterval fromPitch interval =
    case fromPitch of
        Nothing ->
            None

        Just currentPitch ->
            let
                currentDegreeIndex =
                    Degree.indexOf (Pitch.unwrapDegree currentPitch)

                toDegreeIndex =
                    Degree.indexOf (Pitch.unwrapDegree interval.to)
            in
            if toDegreeIndex > currentDegreeIndex then
                AscendTo interval.to

            else
                let
                    fromDegree =
                        Pitch.unwrapDegree interval.from
                in
                if Degree.indexOf fromDegree < currentDegreeIndex then
                    DescendTo interval.from

                else
                    None


unwrapTargetPitch : Movement -> Maybe Pitch
unwrapTargetPitch movement =
    case movement of
        AscendTo pitch ->
            Just pitch

        DescendTo pitch ->
            Just pitch

        None ->
            Nothing


applyAccidental : Scale -> Maybe Accidental -> Movement -> Movement
applyAccidental scale accidental movement =
    case movement of
        AscendTo pitch ->
            AscendTo (Pitch.applyAccidental scale pitch accidental)

        DescendTo pitch ->
            DescendTo (Pitch.applyAccidental scale pitch accidental)

        None ->
            None


{-| Evaluate the movement to verify that it makes sense with respect to the
current pitch. (The target pitch itself will already have been validated by
being constructed.)

We have a problem of an `Interval` in that it's really only conceptually
well-defined for ascending intervals. This causes some minor problems with how
this is used in validating movements involving two different inflected pitches.
This shouldn't _really_ be a problem within the framework of a well-defined
modal framework, but it's a bit inelegant in the abstract.

-}
isValid : Scale -> Pitch -> Movement -> Bool
isValid scale currentPitch movement =
    case movement of
        AscendTo targetPitch ->
            Pitch.compare scale targetPitch currentPitch == GT

        DescendTo targetPitch ->
            Pitch.compare scale targetPitch currentPitch == LT

        None ->
            True
