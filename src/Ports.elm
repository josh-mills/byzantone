port module Ports exposing (pitchDetected, pitchTrackerClicked)

{-| -}


{-| Example port for receiving messages from pitch tracker clicks. We'll want
something that's useful later.
-}
port pitchTrackerClicked : (String -> msg) -> Sub msg


{-| Port for receiving pitch detection events from the pitch tracker component.
When a pitch is detected, it provides the frequency in Hz as a Float. When no
pitch is detected (silence or unclear signal), it returns null which is
converted to Nothing.
-}
port pitchDetected : ({ pitch : Maybe Float } -> msg) -> Sub msg
