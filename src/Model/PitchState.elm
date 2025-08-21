module Model.PitchState exposing
    ( PitchState, initialPitchState
    , IsonStatus(..), ison
    )

{-| User-controlled pitch state.

@docs PitchState, initialPitchState

@docs IsonStatus, ison

-}

import Byzantine.Accidental exposing (Accidental)
import Byzantine.Degree exposing (Degree)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Model.DegreeDataDict as DegreeDataDict exposing (DegreeDataDict)
import Movement exposing (Movement)


type alias PitchState =
    { currentDegree : Maybe Degree
    , currentPitch : Maybe Pitch
    , ison : IsonStatus
    , proposedAccidental : Maybe Accidental
    , appliedAccidentals : DegreeDataDict (Maybe Accidental)
    , proposedMovement : Movement
    }


initialPitchState : PitchState
initialPitchState =
    { currentDegree = Nothing
    , currentPitch = Nothing
    , ison = NoIson
    , proposedAccidental = Nothing
    , appliedAccidentals = DegreeDataDict.init (always Nothing)
    , proposedMovement = Movement.None
    }


{-| A `Selecting` variant for `IsonStatus` is used to indicate when a user is in
the process of changing the current ison. The `Maybe Degree` payload represents
the current ison, which enables continuous pitch when changing the ison.
-}
type IsonStatus
    = NoIson
    | SelectingIson (Maybe Degree)
    | Selected Degree


{-| Current pitch of the ison status.
-}
ison : IsonStatus -> Maybe Pitch
ison isonStatus =
    case isonStatus of
        NoIson ->
            Nothing

        SelectingIson maybeDegree ->
            Maybe.map Pitch.natural maybeDegree

        Selected degree ->
            Just (Pitch.natural degree)
