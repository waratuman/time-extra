module Time.Extra exposing
    ( addDays
    , addDaysZ
    , addHours
    , addMillis
    , addMinutes
    , addSeconds
    , anteMeridiem
    , compare
    , daysInMonth
    , endOfDay
    , endOfMonth
    , endOfWeek
    , epoch
    , fromDateTuple
    , intToMonth
    , monthToInt
    , setDay
    , setHour
    , setMillis
    , setMinute
    , setMonth
    , setSecond
    , setYear
    , startOfDay
    , startOfHour
    , startOfMonth
    , startOfWeek
    , toDateTuple
    , toIso8601Date
    , toIso8601DateTime
    , toIso8601DateTimeUTC
    , toIso8601Time
    , toTimeTuple
    , weekdayFromInt
    , weekdayToInt
    , endOfHour
    )

{-| Library for manipulating the `Posix` type from `elm/time`.

@docs addDays
@docs addDaysZ
@docs addHours
@docs addMillis
@docs addMinutes
@docs addSeconds
@docs anteMeridiem
@docs compare
@docs daysInMonth
@docs endOfDay
@docs endOfMonth
@docs endOfWeek
@docs epoch
@docs fromDateTuple
@docs intToMonth
@docs monthToInt
@docs setDay
@docs setHour
@docs setMillis
@docs setMinute
@docs setMonth
@docs setSecond
@docs setYear
@docs startOfDay
@docs startOfHour
@docs startOfMonth
@docs startOfWeek
@docs toDateTuple
@docs toIso8601Date
@docs toIso8601DateTime
@docs toIso8601DateTimeUTC
@docs toIso8601Time
@docs toTimeTuple
@docs weekdayFromInt
@docs weekdayToInt
@docs endOfHour

-}

import Iso8601
import String
import Task exposing (Task)
import Time exposing (..)
import TimeZone


{-| Unix epoch (1970-01-01T00:00:00Z).
-}
epoch : Posix
epoch =
    millisToPosix 0


{-| Converts a `Posix` into a time tuple (hour, minute, second) for a given `Zone`
-}
toTimeTuple : Zone -> Posix -> ( Int, Int, Int )
toTimeTuple z d =
    ( toHour z d, toMinute z d, toSecond z d )


{-| Converts a `Posix` into a date tuple (year, month, day) for a given `Zone`
-}
toDateTuple : Zone -> Posix -> ( Int, Month, Int )
toDateTuple z d =
    ( toYear z d, toMonth z d, toDay z d )


{-| Converts a date tuple (year, month, day) to a `Posix` for a given `Zone`.
-}
fromDateTuple : Zone -> ( Int, Month, Int ) -> Posix
fromDateTuple z ( y, m, d ) =
    epoch
        |> setYear z y
        |> setMonth z m
        |> setDay z d


{-| Converts a `Posix` into an ISO8601 date for a given `Zone`
-}
toIso8601Date : Zone -> Posix -> String
toIso8601Date z dt =
    String.padLeft 4 '0' (String.fromInt (toYear z dt))
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (toMonth z dt |> monthToInt))
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (toDay z dt))


{-| Converts a `Posix` into an ISO8601 time for a given `Zone`
-}
toIso8601Time : Zone -> Posix -> String
toIso8601Time z dt =
    let
        h =
            toHour z dt

        m =
            toMinute z dt

        s =
            toSecond z dt

        ms =
            toMillis z dt
    in
    String.padLeft 2 '0' (String.fromInt h)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt m)
        ++ (if s == 0 && ms == 0 then
                ""

            else
                ":"
                    ++ String.padLeft 2 '0' (String.fromInt s)
                    ++ (if ms == 0 then
                            ""

                        else
                            "." ++ String.padLeft 3 '0' (String.fromInt ms)
                       )
           )


{-| Converts a `Posix` into an ISO8601 date and time for a given `Zone`
-}
toIso8601DateTime : Zone -> Posix -> String
toIso8601DateTime z dt =
    toIso8601Date z dt
        ++ "T"
        ++ toIso8601Time z dt


{-| Converts a `Posix` into an ISO8601 date and time in UTC
-}
toIso8601DateTimeUTC : Posix -> String
toIso8601DateTimeUTC dt =
    toIso8601Date utc dt
        ++ "T"
        ++ toIso8601Time utc dt
        ++ "Z"


{-| The months of the year.
-}
months : List Month
months =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


{-| Convert an `Int` to a `Month`, with January being 1.
-}
intToMonth : Int -> Maybe Month
intToMonth i =
    case i of
        1 ->
            Just Jan

        2 ->
            Just Feb

        3 ->
            Just Mar

        4 ->
            Just Apr

        5 ->
            Just May

        6 ->
            Just Jun

        7 ->
            Just Jul

        8 ->
            Just Aug

        9 ->
            Just Sep

        10 ->
            Just Oct

        11 ->
            Just Nov

        12 ->
            Just Dec

        _ ->
            Nothing


{-| Convert a `Month` to an `Int`, with January being 1.
-}
monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


{-| Convert a `Weekday` to an `Int`, with Monday being 1 and Sunday being 7.
-}
weekdayToInt : Weekday -> Int
weekdayToInt d =
    case d of
        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6

        Sun ->
            7


{-| Convert an `Int` to a `Weekday`, with Monday being 1 and Sunday being 7.
-}
weekdayFromInt : Int -> Result String Weekday
weekdayFromInt d =
    case d of
        1 ->
            Ok Mon

        2 ->
            Ok Tue

        3 ->
            Ok Wed

        4 ->
            Ok Thu

        5 ->
            Ok Fri

        6 ->
            Ok Sat

        7 ->
            Ok Sun

        _ ->
            Err ("Unknown integer value for weekday: " ++ String.fromInt d)


{-| The number of days in a given year and month.
-}
daysInMonth : Int -> Month -> Int
daysInMonth y m =
    case m of
        Feb ->
            if isleapYear y then
                29

            else
                28

        Apr ->
            30

        Jun ->
            30

        Sep ->
            30

        Nov ->
            30

        _ ->
            31


{-| Set the year of a `Posix` time for a given `Zone`.
-}
setYear : Zone -> Int -> Posix -> Posix
setYear z year t =
    let
        y_ =
            toYear z t

        yms_ =
            msPerYear * (y_ - 1970) + (msPerDay * (leapYearsBefore y_ - leapYearsBefore 1970))

        yms =
            msPerYear * (year - 1970) + (msPerDay * (leapYearsBefore year - leapYearsBefore 1970))
    in
    millisToPosix (posixToMillis t - yms_ + yms)


{-| Set the month of a `Posix` time for a given `Zone`.
-}
setMonth : Zone -> Month -> Posix -> Posix
setMonth z month t =
    let
        y =
            toYear z t

        yms =
            msPerYear * (y - 1970) + (msPerDay * (leapYearsBefore y - leapYearsBefore 1970))

        intMonth =
            monthToInt month

        mms =
            List.foldl
                (\m_ days ->
                    if intMonth > monthToInt m_ then
                        days + daysInMonth y m_

                    else
                        days
                )
                0
                months
                * msPerDay

        dim =
            daysInMonth y month

        d_ =
            toDay z t

        d =
            if d_ > dim then
                dim

            else
                d_

        dms =
            (d - 1) * msPerDay
    in
    yms
        + mms
        + dms
        |> Time.millisToPosix


{-| The number of milliseconds in a hour.
-}
msPerHour : Int
msPerHour =
    3600000


{-| The number of milliseconds in a day.
-}
msPerDay : Int
msPerDay =
    86400000


{-| The number of milliseconds in a normal year (not a leap year).
-}
msPerYear : Int
msPerYear =
    31536000000


{-| Is the given year a leap year?
-}
isleapYear : Int -> Bool
isleapYear y =
    (modBy 4 y == 0) && (modBy 100 y /= 0) || (modBy 400 y == 0)


{-| The number of leap years before a given date.
-}
leapYearsBefore : Int -> Int
leapYearsBefore y1 =
    let
        y =
            y1 - 1
    in
    (y // 4) - (y // 100) + (y // 400)


{-| Set the day of month for a given `Zone`. If the given day is greater then the
number of days in the month it is set to the last day of the month.
-}
setDay : Zone -> Int -> Posix -> Posix
setDay z d t =
    let
        t_ =
            posixToMillis t

        dim =
            daysInMonth (toYear z t) (toMonth z t)

        cd =
            toDay z t

        diff_ =
            (if d <= dim then
                d - cd

             else
                dim - cd
            )
                * msPerDay

        origHour =
            toHour z t

        newHour =
            (t_ + diff_) |> millisToPosix |> toHour z

        diff =
            -- checking for daylight savings change :(
            if newHour == origHour then
                diff_

            else if newHour > origHour then
                diff_ + msPerHour

            else
                diff_ - msPerHour
    in
    t_
        |> (+) diff
        |> millisToPosix


{-| Set the hour of a `Posix` given a `Zone`.
-}
setHour : Zone -> Int -> Posix -> Posix
setHour z h t =
    let
        t_ =
            posixToMillis t

        h_ =
            modBy 24 h

        diff =
            (h_ - toHour z t) * 3600000
    in
    t_ + diff |> millisToPosix


{-| Set the minutes of a `Posix` given a `Zone`.
-}
setMinute : Zone -> Int -> Posix -> Posix
setMinute z m t =
    let
        t_ =
            posixToMillis t

        m_ =
            modBy 60 m

        diff =
            (m_ - toMinute z t) * 60000
    in
    t_ + diff |> millisToPosix


{-| Set the seconds of a `Posix` given a `Zone`.
-}
setSecond : Zone -> Int -> Posix -> Posix
setSecond z s t =
    let
        t_ =
            posixToMillis t

        s_ =
            modBy 60 s

        diff =
            (s_ - toSecond z t) * 1000
    in
    t_ + diff |> millisToPosix


{-| Set the milliseconds of a `Posix` given a `Zone`.
-}
setMillis : Zone -> Int -> Posix -> Posix
setMillis z m t =
    let
        t_ =
            posixToMillis t

        m_ =
            remainderBy 1000 m

        diff =
            m_ - toMillis z t
    in
    t_ + diff |> millisToPosix


{-| Set the `Posix` to the start of the hour in a given `Zone`.
-}
startOfHour : Zone -> Posix -> Posix
startOfHour z t =
    t
        |> setMinute z 0
        |> setSecond z 0
        |> setMillis z 0


{-| Set the `Posix` to the end of the hour in a given `Zone`.
-}
endOfHour : Zone -> Posix -> Posix
endOfHour z t =
    t
        |> setMinute z 59
        |> setSecond z 59
        |> setMillis z 999


{-| Set the `Posix` to the start of the day in a given `Zone`.
-}
startOfDay : Zone -> Posix -> Posix
startOfDay z d =
    d
        |> setHour z 0
        |> setMinute z 0
        |> setSecond z 0
        |> setMillis z 0


{-| Set the `Posix` to the end of the day in a given `Zone`.
-}
endOfDay : Zone -> Posix -> Posix
endOfDay z d =
    d
        |> setHour z 23
        |> setMinute z 59
        |> setSecond z 59
        |> setMillis z 999


{-| Set the `Posix` to the start of the week in a given `Zone` and the start of the
week (a `Weekday`).
-}
startOfWeek : Zone -> Weekday -> Posix -> Posix
startOfWeek z startingWeekday t =
    let
        d_ =
            toWeekday z t |> weekdayToInt

        diff =
            (d_ - (startingWeekday |> weekdayToInt) |> modBy 7) * msPerDay

        t_ =
            t
                |> startOfDay z
                |> posixToMillis
    in
    t_ - diff |> millisToPosix


{-| Set the `Posix` to the end of the week in a given `Zone` and the start of the
week (a `Weekday`).
-}
endOfWeek : Zone -> Weekday -> Posix -> Posix
endOfWeek zone staringWeekday t =
    let
        d_ =
            7
                - 1
                + weekdayToInt staringWeekday
                - (toWeekday zone t |> weekdayToInt)
                |> modBy 7

        diff =
            d_ * msPerDay

        t_ =
            t
                |> endOfDay zone
                |> posixToMillis
    in
    t_ + diff |> millisToPosix


{-| Set the `Posix` to the start of the month in a given `Zone`.
-}
startOfMonth : Zone -> Posix -> Posix
startOfMonth z d =
    d
        |> setDay z 1
        |> setHour z 0
        |> setMinute z 0
        |> setSecond z 0
        |> setMillis z 0


{-| Set the `Posix` to the end of the month in a given `Zone`.
-}
endOfMonth : Zone -> Posix -> Posix
endOfMonth z d =
    d
        |> setDay z (daysInMonth (toYear z d) (toMonth z d))
        -- |> endOfDay z
        |> setHour z 23
        |> setMinute z 59
        |> setSecond z 59
        |> setMillis z 999


{-| Does the given `Posix` occure in the morning of a given `Zone`?
-}
anteMeridiem : Zone -> Posix -> Bool
anteMeridiem z t =
    if toHour z t < 12 then
        True

    else
        False


{-| Add the given milliseconds to the `Posix` time.
-}
addMillis : Int -> Posix -> Posix
addMillis m =
    posixToMillis >> (+) m >> millisToPosix


{-| Add the given seconds to the `Posix` time.
-}
addSeconds : Int -> Posix -> Posix
addSeconds m =
    posixToMillis >> (+) (m * 1000) >> millisToPosix


{-| Add the given minutes to the `Posix` time.
-}
addMinutes : Int -> Posix -> Posix
addMinutes m =
    posixToMillis >> (+) (m * 60000) >> millisToPosix


{-| Add the given hours to the `Posix` time.
-}
addHours : Int -> Posix -> Posix
addHours m =
    posixToMillis >> (+) (m * 3600000) >> millisToPosix


{-| Adds 24 hours to a Posix. Note that this function does not account for
daylight savings time.

Maybe you want the addDaysZ function?

-}
addDays : Int -> Posix -> Posix
addDays d =
    posixToMillis >> (+) (d * 86400000) >> millisToPosix


{-| Adds 24 hours to a `Posix`, attempting to correct for daylight savings time.
-}
addDaysZ : Int -> Zone -> Posix -> Posix
addDaysZ d z t =
    let
        time =
            posixToMillis t |> (+) (d * 86400000) >> millisToPosix
    in
    case toHour z time of
        1 ->
            addHours -1 time

        23 ->
            addHours 1 time

        _ ->
            time


{-| Compares one `Posix` time to another.
-}
compare : Posix -> Posix -> Order
compare a b =
    Basics.compare (posixToMillis a) (posixToMillis b)
