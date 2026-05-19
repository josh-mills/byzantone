module Byzantine.IntervalCharacter exposing
    ( IntervalCharacter(..), allIntervals, basicInterval
    , AscendingChar(..), SomaChar(..), SkipType(..), ascendingStepsString
    , DescendingChar(..), DescendingSteps(..), descendingStepsString
    , applyAccidental
    )

{-| Modeling of intervals.


# Interval Character

@docs IntervalCharacter, allIntervals, basicInterval


# Ascending Characters

@docs AscendingChar, SomaChar, SkipType, ascendingStepsString


# Descending Characters

@docs DescendingChar, DescendingSteps, descendingStepsString


# Accidentals

@docs applyAccidental

-}

import Basics.Extra exposing (flip)
import Byzantine.Accidental exposing (Accidental)


{-| An interval be be either ascending, descending, or unison (ἴσον).
-}
type IntervalCharacter
    = Ascending AscendingChar (Maybe Accidental)
    | Descending DescendingChar (Maybe Accidental)
    | Ison


allIntervals : List IntervalCharacter
allIntervals =
    List.concat
        [ [ Ison ]
        , List.map (flip Ascending Nothing) allAscendingChars
        , List.map (flip Descending Nothing) allDescendingChars
        ]


{-| Ascending characters are a combination of a "soma" character and a "pneuma"
character. The one exception is a kentimata.
-}
type AscendingChar
    = SomaStep SomaChar
    | Kentimata
    | Skip SkipType
    | Ypsili SomaChar AscendingSteps


allAscendingChars : List AscendingChar
allAscendingChars =
    List.concat
        [ List.map SomaStep allSomata
        , [ Kentimata ]
        , List.map Skip allSkips
        , List.concatMap (\soma -> List.map (Ypsili soma) allAscendingSteps) allSomata
        ]


{-| There's also the Ὀξεῖα, which isn't supported by the byzhtml package, so
we're excluding it from this model and just going with ὀλίγον and πεταστή. (The
ὀξεῖα can also be used to support a κέντημα for a skip, which we're leaving out
for simplicity, and since it lacks rendering support at present.)
-}
type SomaChar
    = Oligon
    | Petasti


allSomata : List SomaChar
allSomata =
    [ Oligon, Petasti ]


{-| Ascending movement of two steps can be based on either the ὀλίγον or the
πεταστή. If based on the ὀλίγον, the additional step is indicated by the
κέντιμα, which can be positioned either to to the right or below the ὀλίγον. If
based on the πεταστή, the additional step is indicated by the added ὀλίγον.
-}
type SkipType
    = OligonKentimaBelow
    | OligonKentimaRight
    | PetastiOligon


allSkips : List SkipType
allSkips =
    [ OligonKentimaBelow, OligonKentimaRight, PetastiOligon ]


{-| Descending characters in combination are a simpler problem, because there
aren't multiple possible combinations to represent a single interval.
-}
type DescendingChar
    = Apostrophos
    | Iporroi
    | Elafron
    | SynechesElafron
    | ElafronApostrophos
    | Khamili DescendingSteps


allDescendingChars : List DescendingChar
allDescendingChars =
    [ Apostrophos
    , Iporroi
    , Elafron
    , SynechesElafron
    , ElafronApostrophos
    ]
        ++ List.map Khamili allDescendingSteps


type AscendingSteps
    = UpThree
    | UpFour
    | UpFive
    | UpSix
    | UpSeven
    | UpEight
    | UpNine
    | UpTen
    | UpEleven
    | UpTwelve


allAscendingSteps : List AscendingSteps
allAscendingSteps =
    [ UpThree, UpFour, UpFive, UpSix, UpSeven, UpEight, UpNine, UpTen, UpEleven, UpTwelve ]


ascendingStepsString : AscendingSteps -> String
ascendingStepsString steps =
    case steps of
        UpThree ->
            "3"

        UpFour ->
            "4"

        UpFive ->
            "5"

        UpSix ->
            "6"

        UpSeven ->
            "7"

        UpEight ->
            "8"

        UpNine ->
            "9"

        UpTen ->
            "10"

        UpEleven ->
            "11"

        UpTwelve ->
            "12"


type DescendingSteps
    = DownFour
    | DownFive
    | DownSix
    | DownSeven
    | DownEight
    | DownNine
    | DownTen
    | DownEleven
    | DownTwelve


allDescendingSteps : List DescendingSteps
allDescendingSteps =
    [ DownFour, DownFive, DownSix, DownSeven, DownEight, DownNine, DownTen, DownEleven, DownTwelve ]


descendingStepsString : DescendingSteps -> String
descendingStepsString steps =
    case steps of
        DownFour ->
            "4"

        DownFive ->
            "5"

        DownSix ->
            "6"

        DownSeven ->
            "7"

        DownEight ->
            "8"

        DownNine ->
            "9"

        DownTen ->
            "10"

        DownEleven ->
            "11"

        DownTwelve ->
            "12"


{-| Basic interval character for the given int. Ascending intervals are based on
the oligon. No accidentals.
-}
basicInterval : Int -> Maybe IntervalCharacter
basicInterval int =
    case int of
        0 ->
            Just Ison

        1 ->
            Just (Ascending (SomaStep Oligon) Nothing)

        2 ->
            Just (Ascending (Skip OligonKentimaRight) Nothing)

        3 ->
            Just (Ascending (Ypsili Oligon UpThree) Nothing)

        4 ->
            Just (Ascending (Ypsili Oligon UpFour) Nothing)

        5 ->
            Just (Ascending (Ypsili Oligon UpFive) Nothing)

        6 ->
            Just (Ascending (Ypsili Oligon UpSix) Nothing)

        7 ->
            Just (Ascending (Ypsili Oligon UpSeven) Nothing)

        8 ->
            Just (Ascending (Ypsili Oligon UpEight) Nothing)

        9 ->
            Just (Ascending (Ypsili Oligon UpNine) Nothing)

        10 ->
            Just (Ascending (Ypsili Oligon UpTen) Nothing)

        11 ->
            Just (Ascending (Ypsili Oligon UpEleven) Nothing)

        12 ->
            Just (Ascending (Ypsili Oligon UpTwelve) Nothing)

        other ->
            case negate other of
                1 ->
                    Just (Descending Apostrophos Nothing)

                2 ->
                    Just (Descending Elafron Nothing)

                3 ->
                    Just (Descending ElafronApostrophos Nothing)

                4 ->
                    Just (Descending (Khamili DownFour) Nothing)

                5 ->
                    Just (Descending (Khamili DownFive) Nothing)

                6 ->
                    Just (Descending (Khamili DownSix) Nothing)

                7 ->
                    Just (Descending (Khamili DownSeven) Nothing)

                8 ->
                    Just (Descending (Khamili DownEight) Nothing)

                9 ->
                    Just (Descending (Khamili DownNine) Nothing)

                10 ->
                    Just (Descending (Khamili DownTen) Nothing)

                11 ->
                    Just (Descending (Khamili DownEleven) Nothing)

                12 ->
                    Just (Descending (Khamili DownTwelve) Nothing)

                _ ->
                    Nothing


{-| Add an accidental to an interval character.
-}
applyAccidental : Maybe Accidental -> IntervalCharacter -> IntervalCharacter
applyAccidental maybeAccidental intervalCharacter =
    case intervalCharacter of
        Ascending ascendingChar _ ->
            Ascending ascendingChar maybeAccidental

        Descending descendingChar _ ->
            Descending descendingChar maybeAccidental

        Ison ->
            Ison
