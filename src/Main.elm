module Main exposing (..)

import App exposing (init, update, view, AppState, Flags, Msg(UrlChange))
import Navigation exposing (programWithFlags)


main : Program Flags App.Model App.Msg
main =
    programWithFlags UrlChange
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
