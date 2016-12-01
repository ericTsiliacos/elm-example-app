module Tests exposing (..)

import App exposing (..)
import Http exposing (Error(Timeout))
import Json.Decode
import Json.Encode
import List exposing (head)
import Test exposing (..)
import Expect as To exposing (fail)
import String
import Navigation
import Counter


-- Test Helpers


update_ : Msg -> App.Model -> App.Model
update_ msg model =
    Tuple.first <| update msg model


locationBuilder : Navigation.Location
locationBuilder =
    { href = ""
    , host = ""
    , hostname = ""
    , protocol = ""
    , origin = ""
    , port_ = ""
    , pathname = ""
    , search = ""
    , hash = ""
    , username = ""
    , password = ""
    }



-- Tests


all : Test
all =
    describe "App"
        [ describe "init"
            [ test "loads the initial appState when it exists" <|
                \() ->
                    let
                        appState =
                            { counts = [ 10 ]
                            , numberOfCounters = 0
                            , reverseText = "s"
                            , showText = True
                            }

                        expectedModel =
                            { initialModel
                                | counts = [ 10 ]
                                , numberOfCounters = 0
                                , reverseText = "s"
                                , showText = True
                            }

                        location =
                            { locationBuilder | pathname = "/" }
                    in
                        init
                            { appState = Just appState, apiEndpoint = "" }
                            location
                            |> Tuple.first
                            |> To.equal expectedModel
              --
            , test "returns the defaultModel when there is no initial appState" <|
                \() ->
                    let
                        location =
                            { locationBuilder | pathname = "/" }
                    in
                        init { appState = Nothing, apiEndpoint = "" } location
                            |> Tuple.first
                            |> To.equal initialModel
            ]
          --
        , test "Increment" <|
            \() ->
                let
                    updatedModel =
                        update_ (CountersMsg 0 Counter.Increment) { initialModel | counts = [ 0, 0, 0 ] }
                in
                    updatedModel.counts |> To.equal [ 1, 0, 0 ]
          --
        , describe "Decrement"
            [ test "by 1" <|
                \() ->
                    let
                        currentModel =
                            { initialModel | counts = [ 1, 0, 0 ] }

                        updatedModel =
                            update_ (CountersMsg 0 Counter.Decrement) currentModel
                    in
                        updatedModel |> To.equal { initialModel | counts = [ 0, 0, 0 ] }
              --
            , test "does not decerement past 0" <|
                \() ->
                    let
                        updatedModel =
                            update_ (CountersMsg 0 Counter.Decrement) initialModel
                    in
                        updatedModel |> To.equal initialModel
            ]
          --
        , describe "CountersSliderChange"
            [ test "removes counters when the change is less than the current number of counters" <|
                \() ->
                    let
                        currentModel =
                            { initialModel
                                | counts = [ 1, 1, 1 ]
                                , numberOfCounters = 3
                            }

                        updatedModel =
                            update_ (CountersSliderChange "2") currentModel
                    in
                        updatedModel
                            |> To.equal
                                { initialModel
                                    | counts = [ 1, 1 ]
                                    , numberOfCounters = 2
                                }
              --
            , test "creates counters that are initialized to 0 when the current number of counters is greater" <|
                \() ->
                    let
                        currentModel =
                            { initialModel
                                | counts = [ 1, 1, 1 ]
                                , numberOfCounters = 3
                            }

                        updatedModel =
                            update_ (CountersSliderChange "4") currentModel
                    in
                        updatedModel
                            |> To.equal
                                { initialModel
                                    | counts = [ 1, 1, 1, 0 ]
                                    , numberOfCounters = 4
                                }
            ]
          --
        , test "Reset" <|
            \() ->
                let
                    people =
                        [ { id = 1, name = "batman" } ]

                    currentModel =
                        { initialModel
                            | reverseText = "boo"
                            , showText = False
                            , text = "oob"
                            , counts = [ 1 ]
                            , people = people
                        }

                    updatedModel =
                        update_ Reset currentModel

                    expectedModel =
                        { initialModel | people = people }
                in
                    updatedModel |> To.equal expectedModel
          --
        , test "TextChange" <|
            \() ->
                let
                    updatedModel =
                        update_ (TextChange "batman") initialModel
                in
                    updatedModel
                        |> To.equal
                            { initialModel
                                | reverseText = "namtab"
                                , text = "batman"
                            }
          --
        , describe "ToggleCheckBox"
            [ test "when checked it sets showText to true" <|
                \() ->
                    let
                        updatedModel =
                            update_ (ToggleCheckBox True) initialModel
                    in
                        updatedModel.showText |> To.equal True
              --
            , test "when unchecked it sets showText to false" <|
                \() ->
                    let
                        updatedModel =
                            update_ (ToggleCheckBox False) initialModel
                    in
                        updatedModel.showText |> To.equal False
            ]
          --
        , describe "getting people"
            [ test "when fetching people succeeds, store people on the model" <|
                \() ->
                    let
                        people =
                            [ { id = 1, name = "batman" } ]

                        updatedModel =
                            update_ (GetPeople (Ok people)) initialModel
                    in
                        updatedModel.people
                            |> To.equal
                                [ { id = 1, name = "batman" } ]
              --
            , test "when fetching people fails, it returns the current model" <|
                \() ->
                    let
                        updatedModel =
                            update_ (GetPeople (Err Timeout)) initialModel
                    in
                        updatedModel.getPeopleErrorMsg
                            |> To.equal "Timeout"
            ]
          --
        , test "decoding people" <|
            \() ->
                let
                    peopleJSONString =
                        "{\"data\":[{\"id\":1,\"name\":\"batman\"}]}"

                    peopleResult =
                        Json.Decode.decodeString peopleDecoder peopleJSONString

                    people =
                        case peopleResult of
                            Ok value ->
                                value

                            Err _ ->
                                []
                in
                    people |> To.equal [ { id = 1, name = "batman" } ]
          --
        , test "encoding a model to save to localstorage" <|
            \() ->
                let
                    expectedJSONString =
                        "{\"counts\":[0,0,0],\"numberOfCounters\":0,\"reverseText\":\"\",\"showText\":true}"

                    actualJSONString =
                        encodeModel <| { initialModel | counts = [ 0, 0, 0 ] }
                in
                    actualJSONString |> To.equal expectedJSONString
          --
        , test "decoding the appState" <|
            \() ->
                let
                    appStateJSONString =
                        "{\"counts\":[0,0,0],\"numberOfCounters\":3,\"reverseText\":\"\",\"showText\":true}"

                    appStateResult =
                        Json.Decode.decodeString
                            appStateDecoder
                            appStateJSONString
                in
                    case appStateResult of
                        Ok appState ->
                            appState
                                |> To.equal
                                    { counts = [ 0, 0, 0 ]
                                    , numberOfCounters = 3
                                    , reverseText = ""
                                    , showText = True
                                    }

                        Err withErr ->
                            To.fail withErr
          --
        , describe "LoadLocalStorageAppState"
            [ test "updates the model with the saved appState with decodable appState" <|
                \() ->
                    let
                        appStateJSONString =
                            "{\"counts\":[1,1,1],\"numberOfCounters\":3,\"reverseText\":\"namtab\",\"showText\":false}"

                        updatedModel =
                            update_
                                (LoadLocalStorageAppState appStateJSONString)
                                initialModel
                    in
                        updatedModel
                            |> To.equal
                                { initialModel
                                    | counts = [ 1, 1, 1 ]
                                    , numberOfCounters = 3
                                    , reverseText = "namtab"
                                    , showText = False
                                    , people = []
                                }
              --
            , test "when the data is un-decodable, it returns the same model" <|
                \() ->
                    let
                        appStateJSONString =
                            ""

                        updatedModel =
                            update_
                                (LoadLocalStorageAppState appStateJSONString)
                                initialModel
                    in
                        updatedModel
                            |> To.equal
                                initialModel
            ]
          --
        , describe "UrlChange"
            [ test "/" <|
                \() ->
                    let
                        location =
                            { locationBuilder | pathname = "/" }
                    in
                        update_ (UrlChange location) initialModel
                            |> To.equal { initialModel | currentRoute = Home }
              --
            , test "/first" <|
                \() ->
                    let
                        location =
                            { locationBuilder | pathname = "/first" }
                    in
                        update_ (UrlChange location) initialModel
                            |> To.equal { initialModel | currentRoute = First }
              --
            , test "/second" <|
                \() ->
                    let
                        location =
                            { locationBuilder | pathname = "/second" }
                    in
                        update_ (UrlChange location) initialModel
                            |> To.equal { initialModel | currentRoute = Second }
            ]
        ]
