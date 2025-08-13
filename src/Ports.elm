port module Ports exposing (pitchTrackerClicked)

{-| -}


{-| Example port for receiving messages from pitch tracker clicks. We'll want
something that's useful later.
-}
port pitchTrackerClicked : (String -> msg) -> Sub msg
