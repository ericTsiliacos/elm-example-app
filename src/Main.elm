module Main exposing (..)

import App exposing (init, update, view, AppState)
import Html exposing (programWithFlags)


main : Program (Maybe AppState) App.Model App.Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
