module Model.PitchState exposing (PitchState, initialPitchState)

{-| User-controlled pitch state.
-}

import Byzantine.Degree exposing (Degree)
import Movement exposing (Movement)


type alias PitchState =
    { currentPitch : Maybe Degree
    , proposedMovement : Movement
    }


initialPitchState : PitchState
initialPitchState =
    { currentPitch = Nothing
    , proposedMovement = Movement.None
    }
