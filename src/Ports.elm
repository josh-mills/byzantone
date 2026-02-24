port module Ports exposing (pitchDetected)

{-| -}


{-| Port for receiving pitch detection events from the pitch tracker component.
When a pitch is detected, it provides the frequency in Hz as a Float and the
timestamp in milliseconds as an Int. When no pitch is detected (silence or
unclear signal), the pitch returns null which is converted to Nothing.
-}
port pitchDetected : ({ pitch : Maybe Float, timestamp : Int } -> msg) -> Sub msg
