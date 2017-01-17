module CounterTests exposing (..)

import Test exposing (..)
import Expect as To exposing (fail)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag)
import Counter


all : Test
all =
    describe "Counter"
        [ test "displays the incremented count" <|
            \() ->
                Counter.update Counter.Increment 0
                    |> Counter.view
                    |> Query.fromHtml
                    |> Query.find [ tag "p" ]
                    |> Query.has [ text "1" ]
        ]
