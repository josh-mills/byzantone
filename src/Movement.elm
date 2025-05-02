module Movement exposing (Movement(..), applyAccidental, ofInterval, toPitch)

{-| A representation of motion within intervallic space. This is more a
presentational concern rather than a theoretical concept within the system, so,
for example, there is no purpose in modeling the ison.
-}

import Byzantine.Accidental exposing (Accidental)
import Byzantine.Degree as Degree
import Byzantine.Pitch as Pitch exposing (Interval, Pitch)


type Movement
    = AscendTo Pitch
    | DescendTo Pitch
    | None


ofInterval : Maybe Pitch -> Interval -> Movement
ofInterval fromPitch interval =
    case fromPitch of
        Nothing ->
            None

        Just currentPitch ->
            let
                currentDegree =
                    Pitch.unwrapDegree currentPitch

                toDegree_ =
                    Pitch.unwrapDegree interval.to

                fromDegree_ =
                    Pitch.unwrapDegree interval.from
            in
            if Degree.indexOf toDegree_ > Degree.indexOf currentDegree then
                AscendTo interval.to

            else if Degree.indexOf fromDegree_ < Degree.indexOf currentDegree then
                DescendTo interval.from

            else
                None


toPitch : Movement -> Maybe Pitch
toPitch movement_ =
    case movement_ of
        AscendTo pitch ->
            Just pitch

        DescendTo pitch ->
            Just pitch

        None ->
            Nothing


applyAccidental : Maybe Accidental -> Movement -> Movement
applyAccidental accidental movement =
    case movement of
        AscendTo pitch ->
            AscendTo (Pitch.from accidental (Pitch.unwrapDegree pitch))

        DescendTo pitch ->
            DescendTo (Pitch.from accidental (Pitch.unwrapDegree pitch))

        None ->
            None
