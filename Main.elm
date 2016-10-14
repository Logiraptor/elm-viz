module Main exposing (..)

-- where

import Html
import Html.App
import Html.Events
import Platform.Cmd
import Platform.Sub
import Date
import Time
import Task
import String
import Date.Extra.Duration as Duration
import Date.Extra.Format as Format
import Datatypes exposing (..)
import Date.Extra.Config.Config_en_us as EN
import Viz.LineChart as LineChart
import D3.Axis as Axis


main =
    Html.App.program
        { init = ( state0, Task.perform Now Now Date.now )
        , subscriptions = (\_ -> Platform.Sub.none)
        , update = update
        , view = view
        }


type Msg
    = Now Date.Date


noFx : Model -> ( Model, Cmd Msg )
noFx m =
    ( m, Platform.Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Now date ->
            noFx <| { model | now = date }


view : Model -> Html.Html Msg
view model =
    Html.div []
        (List.map (viewAccount model)
            model.accounts
        )


viewAccount : Model -> Account -> Html.Html Msg
viewAccount model acc =
    let
        upcoming =
            projectTransactions (Duration.add Duration.Year 1 model.now) acc.transactions

        accountSummary =
            summarizeAccount model acc upcoming

        chart =
            LineChart.chart (.date >> Date.toTime) .amount
                |> LineChart.width 800
                |> LineChart.height 300
                |> LineChart.xAxis (Axis.tickFormat (Date.fromTime >> (Format.format EN.config "%b %Y")))
                |> LineChart.yAxis (Axis.tickFormat (Basics.round >> Basics.toString))
    in
        Html.div []
            [ Html.h1 [] [ Html.text (acc.name ++ " - " ++ (formatMoney acc.balance)) ]
            , Html.h2 [] [ Html.text "Upcoming Transactions Chart" ]
            , LineChart.render chart upcoming
            , Html.h2 [] [ Html.text "Account Balance Chart" ]
            , LineChart.render chart accountSummary
            , Html.h2 [] [ Html.text "Transactions" ]
            , Html.ul []
                (List.map viewTransaction acc.transactions)
            , Html.h2 [] [ Html.text "Projection" ]
            , Html.ul []
                (List.map viewUpcoming upcoming)
            ]


summarizeAccount : Model -> Account -> List Upcoming -> List AccountBalance
summarizeAccount model acc upcoming =
    case List.head upcoming of
        Maybe.Nothing ->
            [ { date = model.now, amount = acc.balance } ]

        Just head ->
            List.scanl applyTransaction { date = head.date, amount = acc.balance } upcoming


type alias AccountBalance =
    { date : Date.Date, amount : Float }


applyTransaction : Upcoming -> AccountBalance -> AccountBalance
applyTransaction trans acc =
    { date = trans.date, amount = acc.amount + trans.amount }


viewUpcoming : Upcoming -> Html.Html Msg
viewUpcoming trans =
    Html.li []
        [ Html.text
            (String.join " "
                [ trans.name
                , "is"
                , (formatMoney trans.amount)
                , "on"
                , (formatDate trans.date)
                ]
            )
        ]


viewTransaction : Transaction -> Html.Html Msg
viewTransaction trans =
    Html.li []
        [ Html.text
            (String.join " "
                [ trans.name
                , "is"
                , (formatMoney trans.amount)
                , (formatSchedule trans.schedule)
                ]
            )
        ]


formatMoney : Float -> String
formatMoney money =
    "$" ++ (toString money)


formatSchedule : Schedule -> String
formatSchedule sched =
    formatFreq sched.freq ++ " starting on " ++ formatDate sched.first


formatFreq : Frequency -> String
formatFreq freq =
    case freq of
        Once ->
            "once"

        Every n duration ->
            "every " ++ (toString n) ++ " " ++ (formatDuration n duration)


formatOrdinal : Int -> String
formatOrdinal value =
    let
        hundredRemainder =
            value % 100

        tenRemainder =
            value % 10

        diff =
            hundredRemainder - tenRemainder

        suffix =
            case ( diff, tenRemainder ) of
                ( 10, _ ) ->
                    "th"

                ( _, 1 ) ->
                    "st"

                ( _, 2 ) ->
                    "nd"

                ( _, 3 ) ->
                    "rd"

                ( _, _ ) ->
                    "th"
    in
        (toString value) ++ suffix


formatDuration : Int -> Duration.Duration -> String
formatDuration n d =
    let
        suffix =
            if n > 0 then
                "s"
            else
                ""
    in
        (toString d) ++ suffix


formatDate : Date.Date -> String
formatDate date =
    (toString (Date.month date))
        ++ " "
        ++ (formatOrdinal (Date.day date))
        ++ ", "
        ++ (toString (Date.year date))
