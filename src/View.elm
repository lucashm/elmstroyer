module View exposing (..)
import Html exposing (Html, text, div, img, canvas, button)
import Html.Attributes exposing (..)
import Style exposing (styleMainDiv, styleCanvas)
import Element exposing (..)
import Collage exposing (..)
import Keyboard.Extra exposing (..)
import Model
import Msg



type alias Msg =
    Msg.Msg

type alias Model =
    Model.Model


view : Model.Model -> Html Msg
view model =
    let
        shiftPressed =
            List.member Shift model.pressedKeys

        arrows =
            Keyboard.Extra.arrows model.pressedKeys

        wasd =
            Keyboard.Extra.wasd model.pressedKeys
    in
        div [style styleMainDiv]
        [ -- toHtml (fittedImage 400 400 "http://piq.codeus.net/static/media/userpics/piq_378272_400x400.png")
          div [style styleCanvas]
              [ toHtml (Collage.collage 500 500 [ createBackground, model.player, model.shots ] )
              ]


        ]




createBackground : Collage.Form
createBackground =
    toForm (fittedImage 500 500 "https://media.giphy.com/media/ZiDpJRogbB9V6/giphy.gif")

-- createPlayer =
--     Collage.filled red (Collage.square 30)
