module Tests exposing (..)

import Counter exposing (Msg(Decrement, Increment, TextChange, ToggleCheckBox), init, update)
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "App Update"
        [ test "initial count is set to 0" <|
            \() ->
                Expect.equal init.count 0
        , test "Increment" <|
            \() ->
                Expect.equal (update Increment init).count 1
        , describe "Decrement"
            [ test "by 1" <|
                \() ->
                    Expect.equal (update Decrement { init | count = 1 }) init
            , test "does not decerement past 0" <|
                \() ->
                    Expect.equal (update Decrement init) init
            ]
        , test "TextChange" <|
            \() ->
                Expect.equal (update (TextChange "batman") init).text "namtab"
        , describe "TextChange"
            [ test "when checked it sets showText to true" <|
                \() ->
                    Expect.equal (update (ToggleCheckBox True) init).showText True
            , test "when unchecked it sets showText to false" <|
                \() ->
                    Expect.equal (update (ToggleCheckBox False) init).showText False
            ]
        ]
