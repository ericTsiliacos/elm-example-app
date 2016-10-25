module Tests exposing (..)

import App exposing (Msg(Decrement, Increment), init, update)
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "App Update"
        [ test "initial count is set to 0" <|
            \() ->
                Expect.equal init { count = 0 }
        , test "Increment" <|
            \() ->
                Expect.equal (update Increment init) { count = 1 }
        , describe "Decrement"
            [ test "by 1" <|
                \() ->
                    Expect.equal (update Decrement { count = 1 }) init
            , test "does not decerement past 0" <|
                \() ->
                    Expect.equal (update Decrement init) init
            ]
        ]
