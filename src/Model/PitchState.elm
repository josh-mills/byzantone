module Model.PitchState exposing
    ( PitchState, initialPitchState
    , IsonStatus(..), ison
    , ProposedAccidental(..), unwrapProposedAccidental
    , currentPitch, hasAppliedAccidentals
    )

{-| User-controlled pitch state.

@docs PitchState, initialPitchState

@docs IsonStatus, ison

@docs ProposedAccidental, unwrapProposedAccidental

@docs currentPitch, hasAppliedAccidentals

-}

import Byzantine.Accidental exposing (Accidental)
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Pitch as Pitch exposing (Pitch)
import Model.DegreeDataDict as DegreeDataDict exposing (DegreeDataDict)
import Model.ModeSettings as ModeSettings exposing (ModeSettings)
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


{-| Is an accidental applied to any degree? If not, there is nothing for
`CancelAccidental` to cancel.
-}
hasAppliedAccidentals : PitchState -> Bool
hasAppliedAccidentals pitchState =
    Degree.gamutList
        |> List.any
            (\degree ->
                DegreeDataDict.get degree pitchState.appliedAccidentals /= Nothing
            )


{-| Evaluate the current pitch as a function of the current degree with
reference to the applied accidentals.
-}
currentPitch : ModeSettings -> PitchState -> Maybe Pitch
currentPitch modeSettings pitchState =
    Maybe.map
        (\degree ->
            ModeSettings.pitchFrom modeSettings
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
