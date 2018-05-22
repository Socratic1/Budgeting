port module Main exposing (..)

import Dict exposing (Dict)
import Date exposing (Date)
import AllDict exposing (AllDict)
import Json.Encode as Encode


port updates : Encode.Value -> Cmd msg


type alias Model =
    { accounts : Dict Id Account
    , categories : Dict Id Category
    , supercategories : Dict Id Supercategory
    , transactions : TransactionsByDate
    , budget : Budget
    }


type TransactionsByDate
    = TransactionsByDate (AllDict Date (Dict Id Transaction) String)


type Budget
    = Budget (AllDict Month MonthlyBudget String)


dateToString date =
    toString (Date.year date) ++ "-" ++ monthToStr (Date.month date) ++ "-" ++ (date |> Date.day |> toString |> String.padLeft 2 '0')


monthToStr month =
    case month of
        Date.Jan ->
            "01"

        Date.Feb ->
            "02"

        Date.Mar ->
            "03"

        Date.Apr ->
            "04"

        Date.May ->
            "05"

        Date.Jun ->
            "06"

        Date.Jul ->
            "07"

        Date.Aug ->
            "08"

        Date.Sep ->
            "09"

        Date.Oct ->
            "10"

        Date.Nov ->
            "11"

        Date.Dec ->
            "12"


budgetFromList : List ( Month, MonthlyBudget ) -> Budget
budgetFromList list =
    let
        ord month =
            toString month.year ++ "-" ++ monthToStr month.month
    in
        Budget (AllDict.fromList ord list)


transactionsByDateFromList : List ( Date, Dict Id Transaction ) -> TransactionsByDate
transactionsByDateFromList list =
    TransactionsByDate (AllDict.fromList dateToString list)


transactionsByDateToList : TransactionsByDate -> List ( Date, Dict Id Transaction )
transactionsByDateToList (TransactionsByDate allDict) =
    AllDict.toList allDict


type alias Account =
    String


type alias Supercategory =
    String


type alias Category =
    { name : String
    , supercategory : Id
    }


type alias Transaction =
    { direction : Direction
    , amount : Float
    , account : Id
    , payee : String
    , category : Id
    , memo : Maybe String
    }


type Direction
    = Income
    | Expense


type alias Id =
    String


type alias Month =
    { month : Date.Month
    , year : Int
    }


type alias MonthlyBudget =
    -- Id here represents the Category Id
    Dict Id Amount


type alias Amount =
    Float


init : ( Model, Cmd Msg )
init =
    ( mock, index mock )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


index model =
    let
        encodeDate date =
            dateToString date
                |> Encode.string

        encodeDirection direction =
            (case direction of
                Income ->
                    "Income"

                Expense ->
                    "Expense"
            )
                |> Encode.string

        encodeMemo maybe =
            case maybe of
                Just memo ->
                    Encode.string memo

                Nothing ->
                    Encode.null

        getAccount id =
            model.accounts
                |> Dict.get id
                |> Result.fromMaybe ("There is no account with id" ++ id)

        encodeAccount result =
            case result of
                Ok account ->
                    Encode.string account

                Err message ->
                    Debug.crash message

        getCategory id =
            model.categories
                |> Dict.get id
                |> Result.fromMaybe ("There is no category with id" ++ id)

        encodeCategory result =
            case result of
                Ok category ->
                    Encode.string category.name

                Err message ->
                    Debug.crash message

        applyDirection direction amount =
            amount
                |> case direction of
                    Income ->
                        (*) 1

                    Expense ->
                        (*) -1

        encodeIndex transactions =
            Encode.object
                [ ( "index"
                  , transactions
                        |> transactionsByDateToList
                        |> List.sortBy (\( date, _ ) -> dateToString date)
                        |> List.reverse
                        |> List.map
                            (\( date, transactions ) ->
                                transactions
                                    |> Dict.toList
                                    |> List.map
                                        (\( id, transaction ) ->
                                            Encode.object
                                                [ ( "direction", encodeDirection transaction.direction )
                                                , ( "amount", transaction.amount |> applyDirection transaction.direction |> Encode.float )
                                                , ( "payee", Encode.string transaction.payee )
                                                , ( "memo", encodeMemo transaction.memo )
                                                , ( "account", transaction.account |> getAccount |> encodeAccount )
                                                , ( "category", transaction.category |> getCategory |> encodeCategory )
                                                , ( "date", encodeDate date )
                                                , ( "id", Encode.string id )
                                                ]
                                        )
                            )
                        |> List.concat
                        |> Encode.list
                  )
                ]
    in
        updates (encodeIndex model.transactions)


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }


mock =
    let
        okOrCrash result =
            case result of
                Ok success ->
                    success

                Err message ->
                    Debug.crash message
    in
        { accounts =
            Dict.fromList
                [ ( "samuel.rodney.phillips@gmail.com:VolksbankLahrId", "Volksbank Lahr" )
                , ( "samuel.rodney.phillips@gmail.com:TriodosNLId", "Triodos NL" )
                , ( "samuel.rodney.phillips@gmail.com:CashId", "Cash" )
                ]
        , categories =
            Dict.fromList
                [ ( "TransportationId", { name = "Transportation", supercategory = "ImmediateObligationsId" } )
                , ( "GroceriesId", { name = "Groceries", supercategory = "ImmediateObligationsId" } )
                , ( "samuel.rodney.phillips@gmail.com:MedicalId", { name = "Medical", supercategory = "samuel.rodney.phillips@gmail.com:TrueExpensesId" } )
                , ( "samuel.rodney.phillips@gmail.com:Tech", { name = "Tech", supercategory = "JustForFunId" } )
                ]
        , supercategories =
            Dict.fromList
                [ ( "ImmediateObligationsId", "Immediate Obligations" )
                , ( "samuel.rodney.phillips@gmail.com:Baha'iId", "Baha'i" )
                , ( "JustForFunId", "Just For Fun" )
                ]
        , transactions =
            transactionsByDateFromList
                [ ( "2018-04-20"
                        |> Date.fromString
                        |> okOrCrash
                  , Dict.fromList
                        [ ( "samuel.rodney.phillips@gmail.com:Cash:Transportation:1524353116Id"
                          , { direction = Expense
                            , amount = 4.3
                            , account = "samuel.rodney.phillips@gmail.com:CashId"
                            , category = "TransportationId"
                            , payee = "Deutscher Bahn"
                            , memo = Just "Zell to Offenburg"
                            }
                          )
                        , ( "samuel.rodney.phillips@gmail.com:VolksbankLahr:Transportation:1524215965Id"
                          , { direction = Expense
                            , amount = 61.1
                            , account = "samuel.rodney.phillips@gmail.com:VolksbankLahrId"
                            , category = "TransportationId"
                            , payee = "Deutscher Bahn"
                            , memo = Just "Offenburg to Trier and return"
                            }
                          )
                        ]
                  )
                , ( "2018-04-25"
                        |> Date.fromString
                        |> okOrCrash
                  , Dict.fromList
                        [ ( "samuel.rodney.phillips@gmail.com:VolksbankLahr:Groceries:1524614400Id"
                          , { direction = Expense
                            , amount = 30.31
                            , account = "samuel.rodney.phillips@gmail.com:VolksbankLahrId"
                            , category = "GroceriesId"
                            , payee = "Edeka Zell"
                            , memo = Nothing
                            }
                          )
                        ]
                  )
                , ( "2018-04-27"
                        |> Date.fromString
                        |> okOrCrash
                  , Dict.fromList
                        [ ( "samuel.rodney.phillips@gmail.com:Cash:Groceries:1524614400Id"
                          , { direction = Expense
                            , amount = 7.79
                            , account = "samuel.rodney.phillips@gmail.com:CashId"
                            , category = "GroceriesId"
                            , payee = "Eco Shop Zell"
                            , memo = Nothing
                            }
                          )
                        ]
                  )
                , ( "2018-04-04"
                        |> Date.fromString
                        |> okOrCrash
                  , Dict.fromList
                        [ ( "samuel.rodney.phillips@gmail.com:Cash:Medical:1524614400Id"
                          , { direction = Expense
                            , amount = 126.7
                            , account = "samuel.rodney.phillips@gmail.com:TriodosNLId"
                            , category = "samuel.rodney.phillips@gmail.com:MedicalId"
                            , payee = "Dtizo"
                            , memo = Just "April"
                            }
                          )
                        ]
                  )
                ]
        , budget =
            budgetFromList
                [ ( { month = Date.Apr, year = 2018 }
                  , Dict.fromList
                        [ ( "TransportationID", 100.0 )
                        , ( "GroceriesId", 150.0 )
                        , ( "samuel.rodney.phillips@gmail.com:MedicalId", 126.7 )
                        ]
                  )
                , ( { month = Date.Mar, year = 2018 }, Dict.empty )
                ]
        }
