module Model.CalendarInfo exposing (CalendarInfo, init)

import Date exposing (Date)
import Easter
import Maybe.Extra


type alias CalendarInfo =
    { currentDate : Maybe Date
    , isEastertide : Bool
    }


init : Maybe Date -> CalendarInfo
init currentDate =
    { currentDate = currentDate
    , isEastertide = calculateIsEastertide currentDate
    }


calculateIsEastertide : Maybe Date -> Bool
calculateIsEastertide maybeCurrentDate =
    Maybe.Extra.unwrap False checkDateInEastertidePeriod maybeCurrentDate


checkDateInEastertidePeriod : Date -> Bool
checkDateInEastertidePeriod currentDate =
    let
        paschaDate =
            Easter.easter Easter.Orthodox (Date.year currentDate)

        easterDate =
            Date.fromCalendarDate paschaDate.year paschaDate.month paschaDate.day
    in
    dateIsInPaschalSeason currentDate easterDate


{-| Ascension is 40 days from Pascha, counted inclusively, so the date
difference check must be less than 39, not 40.
-}
dateIsInPaschalSeason : Date -> Date -> Bool
dateIsInPaschalSeason currentDate paschaDate =
    let
        daysAfterEaster =
            Date.diff Date.Days paschaDate currentDate
    in
    daysAfterEaster >= 0 && daysAfterEaster < 39
