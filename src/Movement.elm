module Movement exposing (Movement(..), applyAccidental, isValid, ofInterval, unwrapTargetPitch)

{-| A representation of motion within intervallic space. This is more a
presentational concern rather than a theoretical concept within the system, so,
for example, there is no purpose in modeling the ison.
-}

import Byzantine.Accidental exposing (Accidental)
import Byzantine.Degree as Degree
import Byzantine.Pitch as Pitch exposing (Interval, Pitch)
import Byzantine.Scale exposing (Scale)


type Movement
    = AscendTo Pitch
    | DescendTo Pitch
    | None


{-| Given a starting pitch (i.e., the current pitch) and an interval
representing the step between two adjacent degrees in the scale, what is the
movement from the current pitch to the next pitch represented by that interval?
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

                fromDegree =
                    Pitch.unwrapDegree interval.from
            in
            if toDegreeIndex > currentDegreeIndex then
                AscendTo interval.to

            else if Degree.indexOf fromDegree < currentDegreeIndex then
                DescendTo interval.from

            else
                None


unwrapTargetPitch : Movement -> Maybe Pitch
unwrapTargetPitch movement_ =
    case movement_ of
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
            AscendTo (Pitch.applyAccidental scale accidental pitch)

        DescendTo pitch ->
            DescendTo (Pitch.applyAccidental scale accidental pitch)

        None ->
            None


{-| Evaluate the movement to verify that it makes sense with respect to the
current pitch. (The target pitch itself will already have been validated by
being constructed.)
-}
isValid : Scale -> Pitch -> Movement -> Bool
isValid scale currentPitch movement =
    let
        positionOf =
            Pitch.pitchPosition scale
    in
    case movement of
        AscendTo targetPitch ->
            positionOf targetPitch > positionOf currentPitch

        DescendTo targetPitch ->
            positionOf targetPitch < positionOf currentPitch

        None ->
            True
