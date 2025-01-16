module Movement exposing (Movement(..), ofInterval, toDegree)

import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Pitch exposing (Interval)


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
            if Degree.indexOf interval.to > Degree.indexOf currentDegree then
                AscendTo interval.to

            else if Degree.indexOf interval.from < Degree.indexOf currentDegree then
                DescendTo interval.from

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
