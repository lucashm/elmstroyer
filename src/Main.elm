module Main exposing (..)
import Html exposing (Html)
import Keyboard.Extra exposing (..)
import Time exposing (..)
import Model
import Msg
import View
 -- import Audio



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
        , Time.every ((1000/30)*millisecond) Msg.Tick
        , Time.every ((1000/30)*millisecond) Msg.HitEnemy
        , Time.every (0.5*second) Msg.Spawn
        , Time.every (200*millisecond) Msg.ShootTimer
        ]
