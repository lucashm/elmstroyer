module View exposing (..)
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import Model
import Msg



type alias Msg =
    Msg.Msg

type alias Model =
    Model.Model


view : Model.Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , div [] [ text "Your Elm App is working!" ]
        ]
