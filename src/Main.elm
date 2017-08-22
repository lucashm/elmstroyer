module Main exposing (..)
import Html exposing (Html)
import Keyboard.Extra exposing (..)
import Time exposing (..)
import Model
import Msg
import View



type alias Model =
    Model.Model


type alias Msg =
    Msg.Msg


main : Program Never Model.Model Msg
main =
    Html.program
        { view = View.view
        , init = (Model.init, Cmd.none)
        , update = Msg.update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Msg.KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (50*millisecond) Msg.Tick
        ]
