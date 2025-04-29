module Movement exposing (Movement(..), ofInterval, toDegree)

{-| A representation of motion within intervallic space. This is more a
presentational concern rather than a theoretical concept within the system, so,
for example, there is no purpose in modeling the ison.
-}

import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Pitch as Pitch exposing (Interval)


type Movement
    = AscendTo Degree
    | DescendTo Degree
    | None


ofInterval : Maybe Degree -> Interval -> Movement
ofInterval fromDegree interval =
    case fromDegree of
        Nothing ->
            None

        Just currentDegree ->
            let
                toDegree_ =
                    Pitch.unwrapDegree interval.to

                fromDegree_ =
                    Pitch.unwrapDegree interval.from
            in
            if Degree.indexOf toDegree_ > Degree.indexOf currentDegree then
                AscendTo toDegree_

            else if Degree.indexOf fromDegree_ < Degree.indexOf currentDegree then
                DescendTo fromDegree_

            else
                None


toDegree : Movement -> Maybe Degree
toDegree movement_ =
    case movement_ of
        AscendTo degree ->
            Just degree

        DescendTo degree ->
            Just degree

        None ->
            Nothing
