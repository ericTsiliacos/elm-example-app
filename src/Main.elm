module Main exposing (..)

import App exposing (init, subscriptions, update, view)
import Html exposing (program)


main : Program Never App.Model App.Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
