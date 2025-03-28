module Model.PitchState exposing
    ( PitchState, initialPitchState
    , IsonStatus(..), ison
    )

{-| User-controlled pitch state.

@docs PitchState, initialPitchState

@docs IsonStatus, ison

-}

import Byzantine.Degree exposing (Degree)
import Movement exposing (Movement)


type alias PitchState =
    { currentPitch : Maybe Degree
    , ison : IsonStatus
    , proposedMovement : Movement
    }


initialPitchState : PitchState
initialPitchState =
    { currentPitch = Nothing
    , ison = NoIson
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
ison : PitchState -> Maybe Degree
ison pitchState =
    case pitchState.ison of
        NoIson ->
            Nothing

        SelectingIson maybeDegree ->
            maybeDegree

        Selected degree ->
            Just degree
