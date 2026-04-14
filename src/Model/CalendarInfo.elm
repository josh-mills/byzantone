module Model.CalendarInfo exposing (CalendarInfo, init)

import Date exposing (Date)


type alias CalendarInfo =
    { currentDate : Maybe Date
    }


init : Maybe Date -> CalendarInfo
init currentDate =
    { currentDate = currentDate
    }
