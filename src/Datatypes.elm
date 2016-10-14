module Datatypes exposing (..)

-- where

import Date
import Time
import Date.Extra.Duration as Duration
import Date.Extra.Create as Create


state0 : Model
state0 =
    { accounts =
        [ { name = "Chase"
          , balance = 2000
          , transactions =
                [ { name = "Rent"
                  , amount = -2190
                  , schedule =
                        { freq = Every 1 Duration.Month
                        , first =
                            Create.dateFromFields 2016 Date.May 1 12 0 0 0
                        }
                  }
                , { name = "Car Note"
                  , amount = -300
                  , schedule =
                        { freq = Every 1 Duration.Month
                        , first =
                            Create.dateFromFields 2016 Date.May 23 12 0 0 0
                        }
                  }
                , { name = "Car Insurance"
                  , amount = -150
                  , schedule =
                        { freq = Every 1 Duration.Month
                        , first =
                            Create.dateFromFields 2016 Date.May 26 12 0 0 0
                        }
                  }
                , { name = "Paycheck"
                  , amount = 2300
                  , schedule =
                        { freq = Every 2 Duration.Week
                        , first =
                            Create.dateFromFields 2016 Date.May 1 12 0 0 0
                        }
                  }
                , { name = "Food"
                  , amount = -105
                  , schedule =
                        { freq = Every 1 Duration.Week
                        , first =
                            Create.dateFromFields 2016 Date.May 1 12 0 0 0
                        }
                  }
                ]
          }
        ]
    , now = Date.fromTime 0
    }


type alias Model =
    { accounts : List Account
    , now : Date.Date
    }


type alias Account =
    { name : String
    , balance : Float
    , transactions : List Transaction
    }


type alias Transaction =
    { name : String
    , amount : Float
    , schedule : Schedule
    }


type alias Schedule =
    { freq : Frequency
    , first : Date.Date
    }


type Frequency
    = Once
    | Every Int Duration.Duration


type alias Upcoming =
    { name : String
    , date : Date.Date
    , amount : Float
    }


projectTransactions : Date.Date -> List Transaction -> List Upcoming
projectTransactions end transactions =
    let
        changes =
            List.concatMap (changesForTransaction end) transactions
    in
        List.sortBy (.date >> Date.toTime) changes


changesForTransaction : Date.Date -> Transaction -> List Upcoming
changesForTransaction end last =
    if (Date.toTime last.schedule.first) > (Date.toTime end) then
        []
    else
        let
            head =
                { amount = last.amount, date = last.schedule.first, name = last.name }

            next =
                nextDate last.schedule.freq last.schedule.first

            tail =
                case next of
                    Nothing ->
                        []

                    Just d ->
                        let
                            oldSchedule =
                                last.schedule

                            newSchedule =
                                { oldSchedule | first = d }
                        in
                            changesForTransaction end { last | schedule = newSchedule }
        in
            head :: tail


nextDate : Frequency -> Date.Date -> Maybe Date.Date
nextDate freq date =
    case freq of
        Once ->
            Nothing

        Every n unit ->
            Just (Duration.add unit n date)
