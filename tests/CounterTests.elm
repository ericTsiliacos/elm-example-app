module CounterTests exposing (..)

import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag)
import Counter


suite : Test
suite =
    describe "Counter"
        [ test "displays the incremented count" <|
            \() ->
                0
                    |> Counter.update Counter.Increment
                    |> Counter.view
                    |> Query.fromHtml
                    |> Query.find [ tag "p" ]
                    |> Query.has [ text "1" ]
        ]
