module Tests exposing (..)

import App exposing (Msg(Decrement, GetPeopleFailure, GetPeopleSuccess, Increment, LoadLocalStorageAppState, TextChange, ToggleCheckBox), Person, appStateDecoder, encodeModel, init, initialModel, peopleDecoder, update)
import Http exposing (Error(Timeout))
import Json.Decode
import Json.Encode
import List exposing (head)
import Test exposing (..)
import Expect exposing (fail)
import String


all : Test
all =
    describe "App"
        [ test "initial count is set to 0" <|
            \() ->
                Expect.equal initialModel.count 0
        , test "Increment" <|
            \() ->
                Expect.equal (fst (update Increment initialModel)).count 1
        , describe "Decrement"
            [ test "by 1" <|
                \() ->
                    Expect.equal (fst (update Decrement { initialModel | count = 1 })) initialModel
            , test "does not decerement past 0" <|
                \() ->
                    Expect.equal (fst (update Decrement initialModel)) initialModel
            ]
        , test "TextChange" <|
            \() ->
                Expect.equal (fst (update (TextChange "batman") initialModel)).text "namtab"
        , describe "TextChange"
            [ test "when checked it sets showText to true" <|
                \() ->
                    Expect.equal (fst (update (ToggleCheckBox True) initialModel)).showText True
            , test "when unchecked it sets showText to false" <|
                \() ->
                    Expect.equal (fst (update (ToggleCheckBox False) initialModel)).showText False
            ]
        , describe "getting people"
            [ test "when fetching people succeeds it stores people on the model" <|
                \() ->
                    Expect.equal (fst (update (GetPeopleSuccess [ { id = 1, name = "batman" } ]) initialModel)).people [ { id = 1, name = "batman" } ]
            , test "when fetching people fails, it returns the current model" <|
                \() ->
                    Expect.equal (fst (update (GetPeopleFailure Timeout) initialModel)) initialModel
            ]
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
                    Expect.equal people [ { id = 1, name = "batman" } ]
        , test "encoding a model to save to localstorage (excluding people)" <|
            \() ->
                let
                    expectedJSONString =
                        "{\"count\":0,\"text\":\"\",\"showText\":true}"

                    actualJSONString =
                        encodeModel initialModel
                in
                    Expect.equal actualJSONString expectedJSONString
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
                            Expect.equal appState { count = 0, text = "", showText = True }

                        Err err ->
                            Expect.fail err
        , describe "LoadLocalStorageAppState"
            [ test "when the data is decodable, it updates the model with the saved appState" <|
                \() ->
                    let
                        appStateJSONString =
                            "{\"count\":1,\"text\":\"namtab\",\"showText\":false}"
                    in
                        Expect.equal
                            (fst (update (LoadLocalStorageAppState appStateJSONString) initialModel))
                            { count = 1, text = "namtab", showText = False, people = [] }
            , test "when the data is un-decodable, it returns the same model" <|
                \() ->
                    let
                        appStateJSONString =
                            ""
                    in
                        Expect.equal
                            (fst (update (LoadLocalStorageAppState appStateJSONString) initialModel))
                            initialModel
            ]
        ]
