module Model.PitchState exposing
    ( PitchState, initialPitchState, resetKeepingIson
    , IsonStatus(..), ison
    , ProposedAccidental(..), unwrapProposedAccidental
    , currentPitch
    )

{-| User-controlled pitch state.

@docs PitchState, initialPitchState, resetKeepingIson

@docs IsonStatus, ison

@docs ProposedAccidental, unwrapProposedAccidental

@docs currentPitch

-}

import Byzantine.Accidental exposing (Accidental)
import Byzantine.Degree exposing (Degree)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Byzantine.Scale exposing (Scale)
import Model.DegreeDataDict as DegreeDataDict exposing (DegreeDataDict)
import Movement exposing (Movement)


type alias PitchState =
    { currentDegree : Maybe Degree
    , ison : IsonStatus
    , proposedAccidental : ProposedAccidental
    , appliedAccidentals : DegreeDataDict (Maybe Accidental)
    , proposedMovement : Movement
    }


initialPitchState : PitchState
initialPitchState =
    { currentDegree = Nothing
    , ison = NoIson
    , proposedAccidental = NoProposedAccidental
    , appliedAccidentals = DegreeDataDict.init (always Nothing)
    , proposedMovement = Movement.None
    }


{-| Reset to the initial pitch state, but keep the current ison sounding. Used
when changing audio modes, since the ison is independent of the mode.

An ison selection in progress is settled: if there is a current ison it is
kept as `Selected`, otherwise there is no ison.

-}
resetKeepingIson : PitchState -> PitchState
resetKeepingIson pitchState =
    { initialPitchState
        | ison =
            case pitchState.ison of
                NoIson ->
                    NoIson

                SelectingIson maybeDegree ->
                    Maybe.map Selected maybeDegree
                        |> Maybe.withDefault NoIson

                Selected degree ->
                    Selected degree
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


{-| Evaluate the current pitch as a function of the current degree with
reference to the applied accidentals.
-}
currentPitch : Scale -> PitchState -> Maybe Pitch
currentPitch scale pitchState =
    Maybe.map
        (\degree ->
            Pitch.from scale
                (DegreeDataDict.get degree pitchState.appliedAccidentals)
                degree
        )
        pitchState.currentDegree


type ProposedAccidental
    = Apply Accidental
    | CancelAccidental
    | NoProposedAccidental


unwrapProposedAccidental : ProposedAccidental -> Maybe Accidental
unwrapProposedAccidental proposedAccidental =
    case proposedAccidental of
        Apply accidental ->
            Just accidental

        CancelAccidental ->
            Nothing

        NoProposedAccidental ->
            Nothing
