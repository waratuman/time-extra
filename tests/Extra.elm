module Extra exposing (suite)

import Expect exposing (Expectation)
import Iso8601
import Test exposing (..)
import Time
    exposing
        ( Month(..)
        , Posix
        , Weekday(..)
        , Zone
        , millisToPosix
        , posixToMillis
        , utc
        )
import Time.Extra exposing (..)
import TimeZone


epoch : Posix
epoch =
    millisToPosix 0


nyc : Zone
nyc =
    TimeZone.america__new_york ()


suite : Test
suite =
    describe "Time.Extra"
        [ describe "addDays"
            [ test "epoch + 1" <|
                \_ ->
                    Expect.equal 86400000 (addDays 1 epoch |> posixToMillis)
            , test "epoch - 1" <|
                \_ ->
                    Expect.equal -86400000 (addDays -1 epoch |> posixToMillis)
            ]
        , describe "addDaysZ"
            [ test "epoch + 1" <|
                \_ ->
                    Expect.equal 86400000 (addDaysZ 1 nyc epoch |> posixToMillis)
            , test "epoch - 1" <|
                \_ ->
                    Expect.equal -86400000 (addDaysZ -1 nyc epoch |> posixToMillis)
            , test "daylight savings time in the spring moving forward" <|
                \_ ->
                    let
                        -- 2018-03-10T00:00
                        start =
                            millisToPosix 1520658000000

                        -- 2018-03-11T00:00
                        end =
                            addDaysZ 1 nyc start
                    in
                    Expect.equal "2018-03-11T00:00" (toIso8601DateTime nyc end)
            , test "daylight savings time in the spring moving backward" <|
                \_ ->
                    let
                        -- 2018-03-11T00:00
                        end =
                            millisToPosix 1520744400000

                        -- 2018-03-10T00:00
                        start =
                            addDaysZ -1 nyc end
                    in
                    Expect.equal "2018-03-10T00:00" (toIso8601DateTime nyc start)
            , test "daylight savings time change in fall moving forward" <|
                \_ ->
                    let
                        -- 2018-11-04T00:00
                        start =
                            millisToPosix 1541304000000

                        -- 2018-10-05T00:00
                        end =
                            addDaysZ 1 nyc start
                    in
                    Expect.equal "2018-11-05T00:00" (toIso8601DateTime nyc end)
            , test "daylight savings time change in fall moving backward" <|
                \_ ->
                    let
                        -- 2018-10-05T00:00
                        end =
                            millisToPosix 1541394000000

                        -- 2018-11-04T00:00
                        start =
                            addDaysZ -1 nyc end
                    in
                    Expect.equal "2018-11-04T00:00" (toIso8601DateTime nyc start)
            ]
        , describe "addHours"
            [ test "epoch + 1" <|
                \_ ->
                    Expect.equal 3600000 (addHours 1 epoch |> posixToMillis)
            , test "epoch - 1" <|
                \_ ->
                    Expect.equal -3600000 (addHours -1 epoch |> posixToMillis)
            ]
        , describe "addMillis"
            [ test "epoch + 1" <|
                \_ ->
                    Expect.equal 1 (addMillis 1 epoch |> posixToMillis)
            , test "epoch - 1" <|
                \_ ->
                    Expect.equal -1 (addMillis -1 epoch |> posixToMillis)
            ]
        , describe "addMinutes"
            [ test "epoch + 1" <|
                \_ ->
                    Expect.equal 60000 (addMinutes 1 epoch |> posixToMillis)
            , test "epoch - 1" <|
                \_ ->
                    Expect.equal -60000 (addMinutes -1 epoch |> posixToMillis)
            ]
        , describe "addSeconds"
            [ test "epoch + 1" <|
                \_ ->
                    Expect.equal 1000 (addSeconds 1 epoch |> posixToMillis)
            , test "epoch - 1" <|
                \_ ->
                    Expect.equal -1000 (addSeconds -1 epoch |> posixToMillis)
            ]
        , describe "daysInMonth"
            [ test "Jan" <|
                \_ ->
                    Expect.equal 31 (daysInMonth 1970 Jan)
            , test "Feb" <|
                \_ ->
                    Expect.equal 28 (daysInMonth 1970 Feb)
            , test "Feb in leap year" <|
                \_ ->
                    Expect.equal 29 (daysInMonth 1972 Feb)
            , test "Mar" <|
                \_ ->
                    Expect.equal 31 (daysInMonth 1970 Mar)
            , test "Apr" <|
                \_ ->
                    Expect.equal 30 (daysInMonth 1970 Apr)
            , test "May" <|
                \_ ->
                    Expect.equal 31 (daysInMonth 1970 May)
            , test "Jun" <|
                \_ ->
                    Expect.equal 30 (daysInMonth 1970 Jun)
            , test "Jul" <|
                \_ ->
                    Expect.equal 31 (daysInMonth 1970 Jul)
            , test "Aug" <|
                \_ ->
                    Expect.equal 31 (daysInMonth 1970 Aug)
            , test "Sep" <|
                \_ ->
                    Expect.equal 30 (daysInMonth 1970 Sep)
            , test "Oct" <|
                \_ ->
                    Expect.equal 31 (daysInMonth 1970 Oct)
            , test "Nov" <|
                \_ ->
                    Expect.equal 30 (daysInMonth 1970 Nov)
            , test "Dec" <|
                \_ ->
                    Expect.equal 31 (daysInMonth 1970 Dec)
            ]
        , describe "endOfDay"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1970-01-01T23:59:59.999Z" (endOfDay utc epoch |> toIso8601DateTimeUTC)
            , test "utc 1970-01-02" <|
                \_ ->
                    Expect.equal "1970-01-02T23:59:59.999Z" (endOfDay utc (addDays 1 epoch) |> toIso8601DateTimeUTC)
            , test "nyc" <|
                \_ ->
                    Expect.equal "1970-01-01T04:59:59.999Z" (endOfDay nyc epoch |> toIso8601DateTimeUTC)
            ]
        , describe "endOfMonth"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1970-01-31T23:59:59.999Z" (endOfMonth utc epoch |> toIso8601DateTimeUTC)
            , test "utc 2015-02-01" <|
                \_ ->
                    Expect.equal "2015-02-28T23:59:59.999Z" (endOfMonth utc (setMonth utc Feb epoch |> setYear utc 2015) |> toIso8601DateTimeUTC)
            , test "utc 2016-02-01" <|
                \_ ->
                    Expect.equal "2016-02-29T23:59:59.999Z" (endOfMonth utc (setMonth utc Feb epoch |> setYear utc 2016) |> toIso8601DateTimeUTC)
            , test "nyc" <|
                \_ ->
                    Expect.equal "1970-01-01T04:59:59.999Z" (endOfMonth nyc epoch |> toIso8601DateTimeUTC)
            , test "daylight savings time change in spring" <|
                \_ ->
                    let
                        -- 2018-03-01T00:00
                        start =
                            millisToPosix 1519880400000

                        -- 2018-03-31T23:59:59.999
                        end =
                            endOfMonth nyc start
                    in
                    Expect.equal "2018-03-31T23:59:59.999" (toIso8601DateTime nyc end)
            , test "daylight savings time change in fall" <|
                \_ ->
                    let
                        -- 2018-10-01T00:00
                        start =
                            millisToPosix 1538366400000

                        -- 2018-10-31T23:59:59.999
                        end =
                            endOfMonth nyc start
                    in
                    Expect.equal "2018-10-31T23:59:59.999" (toIso8601DateTime nyc end)
            ]
        , describe "endOfWeek"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1970-01-04T23:59:59.999Z" (endOfWeek utc Mon epoch |> toIso8601DateTimeUTC)
            , test "utc 1970-01-02" <|
                \_ ->
                    Expect.equal "1970-01-04T23:59:59.999Z" (endOfWeek utc Mon (addDays 1 epoch) |> toIso8601DateTimeUTC)
            , test "nyc" <|
                \_ ->
                    Expect.equal "1970-01-05T04:59:59.999Z"
                        (endOfWeek nyc Mon epoch |> toIso8601DateTimeUTC)
            , test
                "with Sunday as the first day of the week"
              <|
                \_ ->
                    Expect.equal "1970-01-03T23:59:59.999Z" (endOfWeek utc Sun epoch |> toIso8601DateTimeUTC)
            ]
        , describe "intToMonth"
            [ test "Jan" <|
                \_ ->
                    Expect.equal (Just Jan) (intToMonth 1)
            , test "Feb" <|
                \_ ->
                    Expect.equal (Just Feb) (intToMonth 2)
            , test "Mar" <|
                \_ ->
                    Expect.equal (Just Mar) (intToMonth 3)
            , test "Apr" <|
                \_ ->
                    Expect.equal (Just Apr) (intToMonth 4)
            , test "May" <|
                \_ ->
                    Expect.equal (Just May) (intToMonth 5)
            , test "Jun" <|
                \_ ->
                    Expect.equal (Just Jun) (intToMonth 6)
            , test "Jul" <|
                \_ ->
                    Expect.equal (Just Jul) (intToMonth 7)
            , test "Aug" <|
                \_ ->
                    Expect.equal (Just Aug) (intToMonth 8)
            , test "Sep" <|
                \_ ->
                    Expect.equal (Just Sep) (intToMonth 9)
            , test "Oct" <|
                \_ ->
                    Expect.equal (Just Oct) (intToMonth 10)
            , test "Nov" <|
                \_ ->
                    Expect.equal (Just Nov) (intToMonth 11)
            , test "Dec" <|
                \_ ->
                    Expect.equal (Just Dec) (intToMonth 12)
            , test "Err" <|
                \_ ->
                    Expect.equal Nothing (intToMonth 13)
            ]
        , describe "monthToInt"
            [ test "Jan" <|
                \_ ->
                    Expect.equal 1 (monthToInt Jan)
            , test "Feb" <|
                \_ ->
                    Expect.equal 2 (monthToInt Feb)
            , test "Mar" <|
                \_ ->
                    Expect.equal 3 (monthToInt Mar)
            , test "Apr" <|
                \_ ->
                    Expect.equal 4 (monthToInt Apr)
            , test "May" <|
                \_ ->
                    Expect.equal 5 (monthToInt May)
            , test "Jun" <|
                \_ ->
                    Expect.equal 6 (monthToInt Jun)
            , test "Jul" <|
                \_ ->
                    Expect.equal 7 (monthToInt Jul)
            , test "Aug" <|
                \_ ->
                    Expect.equal 8 (monthToInt Aug)
            , test "Sep" <|
                \_ ->
                    Expect.equal 9 (monthToInt Sep)
            , test "Oct" <|
                \_ ->
                    Expect.equal 10 (monthToInt Oct)
            , test "Nov" <|
                \_ ->
                    Expect.equal 11 (monthToInt Nov)
            , test "Dec" <|
                \_ ->
                    Expect.equal 12 (monthToInt Dec)
            ]
        , describe "setDay"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1970-01-02T00:00Z"
                        (setDay utc 2 epoch
                            |> toIso8601DateTimeUTC
                        )
            , test "utc 1970-01-04" <|
                \_ ->
                    Expect.equal "1970-01-04T00:00Z"
                        (setDay utc 4 epoch
                            |> toIso8601DateTimeUTC
                        )
            , test "nyc" <|
                \_ ->
                    Expect.equal "1969-12-01T00:00"
                        (setDay nyc 1 epoch
                            |> startOfDay nyc
                            |> toIso8601DateTime nyc
                        )
            ]
        , describe "setHour"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1970-01-01T02:00Z"
                        (setHour utc 2 epoch
                            |> toIso8601DateTimeUTC
                        )
            , test "utc 1970-01-01T04:00" <|
                \_ ->
                    Expect.equal "1970-01-01T04:00Z"
                        (setHour utc 4 epoch
                            |> toIso8601DateTimeUTC
                        )
            , test "nyc" <|
                \_ ->
                    Expect.equal "1969-12-31T06:00Z"
                        (setHour nyc 1 epoch
                            |> toIso8601DateTimeUTC
                        )
            ]
        , test "setMillis" <|
            \_ ->
                Expect.equal 999 (setMillis utc 999 epoch |> posixToMillis)
        , describe "setMinute"
            [ test "in utc" <|
                \_ ->
                    Expect.equal 60000 (setMinute utc 1 epoch |> posixToMillis)
            , test "in nyc" <|
                \_ ->
                    Expect.equal 60000 (setMinute nyc 1 epoch |> posixToMillis)
            ]
        , describe "setMonth"
            [ test "Feb in utc" <|
                \_ ->
                    Expect.equal "1970-02-01T00:00Z" (setMonth utc Feb epoch |> toIso8601DateTimeUTC)
            , test "Feb in nyc Dec 1969" <|
                \_ ->
                    Expect.equal "1969-02-28T00:00Z" (setMonth nyc Feb epoch |> toIso8601DateTimeUTC)
            , test "Feb in nyc Jan 1st, 1970" <|
                \_ ->
                    Expect.equal "1970-02-01T00:00Z" (setMonth nyc Feb (addHours 5 epoch) |> toIso8601DateTimeUTC)
            , test "Mar in utc" <|
                \_ ->
                    Expect.equal "1970-03-01T00:00Z" (setMonth utc Mar epoch |> toIso8601DateTimeUTC)
            , test "Apr in utc" <|
                \_ ->
                    Expect.equal "1970-04-01T00:00Z" (setMonth utc Apr epoch |> toIso8601DateTimeUTC)
            , test "May in utc" <|
                \_ ->
                    Expect.equal "1970-05-01T00:00Z" (setMonth utc May epoch |> toIso8601DateTimeUTC)
            , test "Jun in utc" <|
                \_ ->
                    Expect.equal "1970-06-01T00:00Z" (setMonth utc Jun epoch |> toIso8601DateTimeUTC)
            , test "Jul in utc" <|
                \_ ->
                    Expect.equal "1970-07-01T00:00Z" (setMonth utc Jul epoch |> toIso8601DateTimeUTC)
            , test "Aug in utc" <|
                \_ ->
                    Expect.equal "1970-08-01T00:00Z" (setMonth utc Aug epoch |> toIso8601DateTimeUTC)
            , test "Sep in utc" <|
                \_ ->
                    Expect.equal "1970-09-01T00:00Z" (setMonth utc Sep epoch |> toIso8601DateTimeUTC)
            , test "Oct in utc" <|
                \_ ->
                    Expect.equal "1970-10-01T00:00Z" (setMonth utc Oct epoch |> toIso8601DateTimeUTC)
            , test "Nov in utc" <|
                \_ ->
                    Expect.equal "1970-11-01T00:00Z" (setMonth utc Nov epoch |> toIso8601DateTimeUTC)
            , test "Dec in utc" <|
                \_ ->
                    Expect.equal "1970-12-01T00:00Z" (setMonth utc Dec epoch |> toIso8601DateTimeUTC)
            ]
        , describe "setSecond"
            [ test "30" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00:30Z" (setSecond utc 30 epoch |> toIso8601DateTimeUTC)
            , test "00" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z"
                        (setSecond utc
                            0
                            (posixToMillis epoch + 30000 |> millisToPosix)
                            |> toIso8601DateTimeUTC
                        )
            , test "60" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z"
                        (setSecond utc 60 epoch |> toIso8601DateTimeUTC)
            ]
        , describe "setYear"
            [ test "1969" <|
                \_ ->
                    Expect.equal "1969-01-01T00:00Z"
                        (setYear utc 1969 epoch |> toIso8601DateTimeUTC)
            , test "1970 nyc" <|
                \_ ->
                    -- epoch in America/New_York is 1969-12-31
                    Expect.equal "1971-01-01T00:00Z"
                        (setYear nyc 1970 epoch |> toIso8601DateTimeUTC)
            , test "2000 nyc" <|
                \_ ->
                    Expect.equal "2000-01-01T00:00Z"
                        (setYear utc 2000 epoch |> toIso8601DateTimeUTC)
            ]
        , describe "startOfDay"
            [ test "in utc from epoch" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z"
                        (startOfDay utc epoch |> toIso8601DateTimeUTC)
            , test "in nyc from epoch" <|
                \_ ->
                    Expect.equal "1969-12-31T05:00Z"
                        (startOfDay nyc epoch |> toIso8601DateTimeUTC)
            ]
        , describe "startOfHour"
            [ test "in utc" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z"
                        (startOfHour utc (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            , test "in utc 2" <|
                \_ ->
                    Expect.equal "1970-01-01T01:00Z"
                        (startOfHour utc (addMinutes 90 epoch) |> toIso8601DateTimeUTC)
            , test "in nyc" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z"
                        (startOfHour nyc (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            ]
        , describe "endOfHour"
            [ test "in utc" <|
                \_ ->
                    Expect.equal "1970-01-01T00:59:59.999Z"
                        (endOfHour utc (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            , test "in utc 2" <|
                \_ ->
                    Expect.equal "1970-01-01T01:59:59.999Z"
                        (endOfHour utc (addMinutes 90 epoch) |> toIso8601DateTimeUTC)
            , test "in nyc" <|
                \_ ->
                    Expect.equal "1970-01-01T00:59:59.999Z"
                        (endOfHour nyc (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            ]
        , describe "startOfMonth"
            [ test "in utc" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z"
                        (startOfMonth utc (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            , test "in utc Jan 15th" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z"
                        (startOfMonth utc (addDays 15 epoch) |> toIso8601DateTimeUTC)
            , test "in nyc" <|
                \_ ->
                    Expect.equal "1969-12-01T05:00Z"
                        (startOfMonth nyc (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            , test "Feb in nyc" <|
                \_ ->
                    Expect.equal "1969-02-01T05:00Z"
                        (startOfMonth nyc (setMonth nyc Feb epoch) |> toIso8601DateTimeUTC)
            , test "daylight savings time" <|
                \_ ->
                    let
                        -- 2018-04-01T00:00
                        a =
                            millisToPosix 1522555200000

                        -- 2018-03-31T23:59:59.999
                        end =
                            startOfMonth nyc a |> addMillis -1

                        -- 2018-03-01T00:00
                        start =
                            startOfMonth nyc end
                    in
                    Expect.equal "2018-03-01T00:00" (toIso8601DateTime nyc start)
            , test "daylight savings time change in fall" <|
                \_ ->
                    let
                        -- 2018-10-31T23:59:59.999
                        end =
                            millisToPosix 1541044799999

                        -- 2018-10-31T23:59:59.999
                        start =
                            endOfMonth nyc end
                    in
                    Expect.equal "2018-10-31T23:59:59.999" (toIso8601DateTime nyc end)
            ]
        , describe "startOfWeek"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1969-12-29T00:00Z"
                        (startOfWeek utc Mon (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            , test "nyc" <|
                \_ ->
                    Expect.equal "1969-12-29T05:00Z"
                        (startOfWeek nyc Mon (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            , test "with Sunday as the start of the week in nyc" <|
                \_ ->
                    Expect.equal "1969-12-28T05:00Z"
                        (startOfWeek nyc Sun (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            , test "with Sunday as the start of the week in utc" <|
                \_ ->
                    Expect.equal "1969-12-28T00:00Z"
                        (startOfWeek utc Sun (addMinutes 30 epoch) |> toIso8601DateTimeUTC)
            ]
        , describe "toDateTuple"
            [ test "utc" <|
                \_ ->
                    Expect.equal ( 1970, Jan, 1 ) (toDateTuple utc epoch)
            , test "nyc" <|
                \_ ->
                    Expect.equal ( 1969, Dec, 31 ) (toDateTuple nyc epoch)
            ]
        , describe "toIso8601Date"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1970-01-01" (epoch |> toIso8601Date utc)
            , test "nyc" <|
                \_ ->
                    Expect.equal "1969-12-31" (epoch |> toIso8601Date nyc)
            ]
        , describe "toIso8601DateTime"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00" (epoch |> toIso8601DateTime utc)
            , test "utc w/ millis" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00:00.001" (epoch |> addMillis 1 |> toIso8601DateTime utc)
            , test "nyc" <|
                \_ ->
                    Expect.equal "1969-12-31T19:00" (epoch |> toIso8601DateTime nyc)
            ]
        , describe "toIso8601DateTimeUTC"
            [ test "utc" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z" (epoch |> toIso8601DateTimeUTC)
            , test "utc w/ millis" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00:00.001Z" (epoch |> addMillis 1 |> toIso8601DateTimeUTC)
            , test "nyc" <|
                \_ ->
                    Expect.equal "1970-01-01T00:00Z" (epoch |> toIso8601DateTimeUTC)
            ]
        , describe "toIso8601Time"
            [ test "utc" <|
                \_ ->
                    Expect.equal "00:00" (epoch |> toIso8601Time utc)
            , test "utc w/ millis" <|
                \_ ->
                    Expect.equal "00:00:00.001" (epoch |> addMillis 1 |> toIso8601Time utc)
            , test "nyc" <|
                \_ ->
                    Expect.equal "19:00" (epoch |> toIso8601Time nyc)
            ]
        , describe "toTimeTuple"
            [ test "utc" <|
                \_ ->
                    Expect.equal ( 0, 0, 0 ) (toTimeTuple utc epoch)
            , test "nyc" <|
                \_ ->
                    Expect.equal ( 19, 0, 0 ) (toTimeTuple nyc epoch)
            ]
        , describe "weekdayFromInt"
            [ test "Mon" <|
                \_ ->
                    Expect.equal (Ok Mon) (weekdayFromInt 1)
            , test "Tue" <|
                \_ ->
                    Expect.equal (Ok Tue) (weekdayFromInt 2)
            , test "Wed" <|
                \_ ->
                    Expect.equal (Ok Wed) (weekdayFromInt 3)
            , test "Thu" <|
                \_ ->
                    Expect.equal (Ok Thu) (weekdayFromInt 4)
            , test "Fri" <|
                \_ ->
                    Expect.equal (Ok Fri) (weekdayFromInt 5)
            , test "Sat" <|
                \_ ->
                    Expect.equal (Ok Sat) (weekdayFromInt 6)
            , test "Sun" <|
                \_ ->
                    Expect.equal (Ok Sun) (weekdayFromInt 7)
            , test "Err" <|
                \_ ->
                    Expect.equal (Err "Unknown integer value for weekday: 8") (weekdayFromInt 8)
            ]
        , describe "weekdayToInt"
            [ test "Mon" <|
                \_ ->
                    Expect.equal 1 (weekdayToInt Mon)
            , test "Tue" <|
                \_ ->
                    Expect.equal 2 (weekdayToInt Tue)
            , test "Wed" <|
                \_ ->
                    Expect.equal 3 (weekdayToInt Wed)
            , test "Thu" <|
                \_ ->
                    Expect.equal 4 (weekdayToInt Thu)
            , test "Fri" <|
                \_ ->
                    Expect.equal 5 (weekdayToInt Fri)
            , test "Sat" <|
                \_ ->
                    Expect.equal 6 (weekdayToInt Sat)
            , test "Sun" <|
                \_ ->
                    Expect.equal 7 (weekdayToInt Sun)
            ]
        ]
