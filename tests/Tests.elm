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
        [ test "initial count is set to 0" <|
            \() ->
                initialModel.count |> To.equal 0
          --
        , describe "init"
            [ test "loads the initial appState when it exists" <|
                \() ->
                    let
                        appState =
                            { count = 10, text = "s", showText = True }

                        expectedModel =
                            { initialModel | count = 10, text = "s", showText = True }

                        location =
                            { locationBuilder | pathname = "/" }
                    in
                        init { appState = Just appState, apiEndpoint = "" } location
                            |> Tuple.first
                            |> To.equal expectedModel
              --
            , test "returns the defaultModel when thre is no initial appState" <|
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
                        update_ Increment initialModel
                in
                    updatedModel.count |> To.equal 1
          --
        , test "Reset" <|
            \() ->
                let
                    people =
                        [ { id = 1, name = "batman" } ]

                    currentModel =
                        { initialModel | text = "boo", showText = False, count = 1, people = people }

                    updatedModel =
                        update_ Reset currentModel

                    expectedModel =
                        { initialModel | people = people }
                in
                    updatedModel |> To.equal expectedModel
          --
        , describe "Decrement"
            [ test "by 1" <|
                \() ->
                    let
                        currentModel =
                            { initialModel | count = 1 }

                        updatedModel =
                            update_ Decrement currentModel
                    in
                        updatedModel |> To.equal initialModel
              --
            , test "does not decerement past 0" <|
                \() ->
                    let
                        updatedModel =
                            update_ Decrement initialModel
                    in
                        updatedModel |> To.equal initialModel
            ]
          --
        , test "TextChange" <|
            \() ->
                let
                    updatedModel =
                        update_ (TextChange "batman") initialModel
                in
                    updatedModel.text |> To.equal "namtab"
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
            [ test "when fetching people succeeds it stores people on the model" <|
                \() ->
                    let
                        people =
                            [ { id = 1, name = "batman" } ]

                        updatedModel =
                            update_ (GetPeople (Ok people)) initialModel
                    in
                        updatedModel.people |> To.equal [ { id = 1, name = "batman" } ]
              --
            , test "when fetching people fails, it returns the current model" <|
                \() ->
                    let
                        updatedModel =
                            update_ (GetPeople (Err Timeout)) initialModel
                    in
                        updatedModel.getPeopleErrorMsg |> To.equal (Just "Timeout")
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
        , test "encoding a model to save to localstorage (excluding people)" <|
            \() ->
                let
                    expectedJSONString =
                        "{\"count\":0,\"text\":\"\",\"showText\":true}"

                    actualJSONString =
                        encodeModel initialModel
                in
                    actualJSONString |> To.equal expectedJSONString
          --
        , test "decoding the appState" <|
            \() ->
                let
                    appStateJSONString =
                        "{\"count\":0,\"text\":\"\",\"showText\":true}"

                    appStateResult =
                        Json.Decode.decodeString appStateDecoder appStateJSONString
                in
                    case appStateResult of
                        Ok appState ->
                            appState |> To.equal { count = 0, text = "", showText = True }

                        Err withErr ->
                            To.fail withErr
          --
        , describe "LoadLocalStorageAppState"
            [ test "when the data is decodable, it updates the model with the saved appState" <|
                \() ->
                    let
                        appStateJSONString =
                            "{\"count\":1,\"text\":\"namtab\",\"showText\":false}"

                        updatedModel =
                            update_ (LoadLocalStorageAppState appStateJSONString) initialModel
                    in
                        updatedModel
                            |> To.equal
                                { initialModel | count = 1, text = "namtab", showText = False, people = [] }
              --
            , test "when the data is un-decodable, it returns the same model" <|
                \() ->
                    let
                        appStateJSONString =
                            ""

                        updatedModel =
                            update_ (LoadLocalStorageAppState appStateJSONString) initialModel
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
