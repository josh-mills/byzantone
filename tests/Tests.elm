module Tests exposing (..)

import Array
import Byzantine.Accidental as Accidental exposing (Accidental(..))
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Pitch as Pitch
import Byzantine.Scale as Scale exposing (Scale(..))
import Expect
import List.Extra
import Model.DegreeDataDict as DegreeDataDict
import Result
import Test exposing (Test, describe, test)


degreeTests : Test
degreeTests =
    describe "Test Degree logic"
        [ describe "indexOf is correct"
            (Degree.gamut
                |> Array.toList
                |> List.indexedMap
                    (\i degree ->
                        test ("getting " ++ String.fromInt i) <|
                            \_ ->
                                Expect.equal (Just degree)
                                    (Array.get (Degree.indexOf degree) Degree.gamut)
                    )
            )
        , describe "gamut is exhaustive" <|
            [ test "length is correct" <|
                \_ ->
                    Expect.equal
                        (List.length gamutBuilder)
                        (Array.length Degree.gamut)
            , test "contents match" <|
                \_ ->
                    Expect.equal
                        (Array.toList Degree.gamut)
                        gamutBuilder
            ]
        , describe "all pitches have a pitch position" <|
            List.concatMap
                (\scale ->
                    List.map
                        (\degree ->
                            test (Scale.name scale ++ " pitch position for " ++ Degree.toString degree ++ " is not negative") <|
                                \_ ->
                                    Expect.greaterThan -1 (Pitch.pitchPosition scale (Pitch.natural degree))
                        )
                        gamutBuilder
                )
                Scale.all
        ]


gamutBuilder : List Degree
gamutBuilder =
    let
        next : List Degree -> List Degree
        next degrees =
            case List.head degrees of
                Nothing ->
                    GA :: degrees |> next

                Just GA ->
                    DI :: degrees |> next

                Just DI ->
                    KE :: degrees |> next

                Just KE ->
                    Zo :: degrees |> next

                Just Zo ->
                    Ni :: degrees |> next

                Just Ni ->
                    Pa :: degrees |> next

                Just Pa ->
                    Bou :: degrees |> next

                Just Bou ->
                    Ga :: degrees |> next

                Just Ga ->
                    Di :: degrees |> next

                Just Di ->
                    Ke :: degrees |> next

                Just Ke ->
                    Zo_ :: degrees |> next

                Just Zo_ ->
                    Ni_ :: degrees |> next

                Just Ni_ ->
                    Pa_ :: degrees |> next

                Just Pa_ ->
                    Bou_ :: degrees |> next

                Just Bou_ ->
                    Ga_ :: degrees |> next

                Just Ga_ ->
                    degrees
    in
    next [] |> List.reverse


accidentalTests : Test
accidentalTests =
    describe "Accidental Tests"
        [ test "Accidental.all is complete and unique" <|
            \_ ->
                Expect.equalLists accidentalBuilder Accidental.all
        , describe "accidentals in the Accidental.all function are in ascending order" <|
            (Accidental.all
                |> List.map Accidental.moriaAdjustment
                |> List.Extra.uniquePairs
                |> List.map
                    (\( a, b ) ->
                        test (String.fromInt a ++ " is less than " ++ String.fromInt b) <|
                            \_ -> Expect.lessThan b a
                    )
            )
        ]


accidentalBuilder : List Accidental
accidentalBuilder =
    let
        next : List Accidental -> List Accidental
        next accidentals =
            case List.head accidentals of
                Nothing ->
                    Flat8 :: accidentals |> next

                Just Flat8 ->
                    Flat6 :: accidentals |> next

                Just Flat6 ->
                    Flat4 :: accidentals |> next

                Just Flat4 ->
                    Flat2 :: accidentals |> next

                Just Flat2 ->
                    Sharp2 :: accidentals |> next

                Just Sharp2 ->
                    Sharp4 :: accidentals |> next

                Just Sharp4 ->
                    Sharp6 :: accidentals |> next

                Just Sharp6 ->
                    Sharp8 :: accidentals |> next

                Just Sharp8 ->
                    accidentals
    in
    next [] |> List.reverse


degreeDataDictTests : Test
degreeDataDictTests =
    describe "DegreeDataDict Tests"
        [ describe "init with identity maps degrees to themselves" <|
            let
                dict =
                    DegreeDataDict.init identity
            in
            List.map
                (\degree ->
                    test (Degree.toString degree ++ " maps to itself") <|
                        \_ ->
                            Expect.equal degree (DegreeDataDict.get degree dict)
                )
                Degree.gamutList
        , test "init creates dict with correct values" <|
            \_ ->
                let
                    dict =
                        DegreeDataDict.init (Degree.indexOf >> String.fromInt)
                in
                Expect.equal "5" (DegreeDataDict.get Pa dict)
        , test "set updates correct degree" <|
            \_ ->
                let
                    dict =
                        DegreeDataDict.init (always 0)
                            |> DegreeDataDict.set Di 42
                in
                Expect.all
                    [ \d -> Expect.equal 42 (DegreeDataDict.get Di d)
                    , \d -> Expect.equal 0 (DegreeDataDict.get Pa d)
                    ]
                    dict
        , test "get returns correct values for all degrees" <|
            \_ ->
                let
                    dict =
                        DegreeDataDict.init Degree.toString
                in
                Expect.all
                    [ \d -> Expect.equal "GA" (DegreeDataDict.get GA d)
                    , \d -> Expect.equal "Zo" (DegreeDataDict.get Zo d)
                    , \d -> Expect.equal "Pa" (DegreeDataDict.get Pa d)
                    , \d -> Expect.equal "Ga_" (DegreeDataDict.get Ga_ d)
                    ]
                    dict
        ]


pitchTests : Test
pitchTests =
    describe "Pitch Tests"
        [ describe "Encode/Decode Roundtrip Tests"
            [ describe "Natural Pitches"
                (List.concatMap
                    (\scale ->
                        List.map
                            (\degree ->
                                let
                                    pitch =
                                        Pitch.natural degree
                                in
                                test (Scale.name scale ++ " " ++ "Natural " ++ Pitch.toString pitch) <|
                                    \_ ->
                                        pitch
                                            |> Pitch.encode
                                            |> Pitch.decode scale
                                            |> Expect.equal (Ok pitch)
                            )
                            Degree.gamutList
                    )
                    Scale.all
                )
            , describe "Inflected Pitches"
                (List.concatMap
                    (\scale ->
                        List.concatMap
                            (\degree ->
                                List.filterMap
                                    (\accidental ->
                                        Pitch.inflected scale accidental degree
                                            |> Result.toMaybe
                                            |> Maybe.map
                                                (\pitch ->
                                                    test (Scale.name scale ++ " " ++ Pitch.toString pitch) <|
                                                        \_ ->
                                                            pitch
                                                                |> Pitch.encode
                                                                |> Pitch.decode scale
                                                                |> Expect.equal (Ok pitch)
                                                )
                                    )
                                    Accidental.all
                            )
                            Degree.gamutList
                    )
                    Scale.all
                )
            ]
        , describe "Scale Encode/Decode Tests"
            [ test "Scale.encode/decode roundtrip for Diatonic" <|
                \_ ->
                    Diatonic
                        |> Scale.encode
                        |> Scale.decode
                        |> Expect.equal (Ok Diatonic)
            , test "Scale.encode/decode roundtrip for Enharmonic" <|
                \_ ->
                    Enharmonic
                        |> Scale.encode
                        |> Scale.decode
                        |> Expect.equal (Ok Enharmonic)
            , test "Scale.encode/decode roundtrip for SoftChromatic" <|
                \_ ->
                    SoftChromatic
                        |> Scale.encode
                        |> Scale.decode
                        |> Expect.equal (Ok SoftChromatic)
            , test "Scale.encode/decode roundtrip for HardChromatic" <|
                \_ ->
                    HardChromatic
                        |> Scale.encode
                        |> Scale.decode
                        |> Expect.equal (Ok HardChromatic)
            , test "Scale.decode returns error for invalid input" <|
                \_ ->
                    Scale.decode "invalid"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "Scale.encode produces the expected string for each scale" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "ds" (Scale.encode Diatonic)
                        , \_ -> Expect.equal "dh" (Scale.encode Enharmonic)
                        , \_ -> Expect.equal "cs" (Scale.encode SoftChromatic)
                        , \_ -> Expect.equal "ch" (Scale.encode HardChromatic)
                        ]
                        ()
            ]
        , describe "EncodeWithScale/DecodeWithScale Roundtrip Tests"
            [ describe "Natural Pitches With Scale"
                (List.concatMap
                    (\scale ->
                        List.map
                            (\degree ->
                                let
                                    pitch =
                                        Pitch.natural degree
                                in
                                test (Scale.name scale ++ " " ++ "Natural " ++ Pitch.toString pitch) <|
                                    \_ ->
                                        pitch
                                            |> Pitch.encodeWithScale scale
                                            |> Pitch.decodeWithScale
                                            |> Expect.equal (Ok pitch)
                            )
                            Degree.gamutList
                    )
                    Scale.all
                )
            , describe "Inflected Pitches With Scale"
                (List.concatMap
                    (\scale ->
                        List.concatMap
                            (\degree ->
                                List.filterMap
                                    (\accidental ->
                                        Pitch.inflected scale accidental degree
                                            |> Result.toMaybe
                                            |> Maybe.map
                                                (\pitch ->
                                                    test (Scale.name scale ++ " " ++ Pitch.toString pitch) <|
                                                        \_ ->
                                                            pitch
                                                                |> Pitch.encodeWithScale scale
                                                                |> Pitch.decodeWithScale
                                                                |> Expect.equal (Ok pitch)
                                                )
                                    )
                                    Accidental.all
                            )
                            Degree.gamutList
                    )
                    Scale.all
                )
            ]
        , describe "DecodeWithScale Error Cases"
            [ test "DecodeWithScale returns error for invalid scale code" <|
                \_ ->
                    "invalid|n|Pa"
                        |> Pitch.decodeWithScale
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "DecodeWithScale returns error for invalid format" <|
                \_ ->
                    "ds"
                        |> Pitch.decodeWithScale
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "DecodeWithScale returns error for invalid degree in pitch" <|
                \_ ->
                    "ds|n|invalid"
                        |> Pitch.decodeWithScale
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "DecodeWithScale returns error for invalid accidental in pitch" <|
                \_ ->
                    "ds|i|Pa|invalid"
                        |> Pitch.decodeWithScale
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "DecodeWithScale returns error for incompatible accidental" <|
                \_ ->
                    -- Attempting to use an accidental that isn't valid for the scale/degree
                    "ds|i|Pa|f8"
                        |> Pitch.decodeWithScale
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        ]
