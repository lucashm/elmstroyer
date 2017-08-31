module View exposing (..)
import Html exposing (Html, text, div, img, canvas, button)
import Html.Attributes exposing (..)
import Style exposing (styleMainDiv, styleCanvas)
import Element exposing (..)
import Collage exposing (..)
import List exposing (..)
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

        ( shoots, coordinates ) = unzip model.shoots
        (formShoots, hitboxShoots) = unzip shoots

        ( enemies, coordinatesE ) = unzip model.enemies
        (formEnemies, hitboxEnemies) = unzip enemies

        (player, collision) = model.player

    in
        div [style styleMainDiv]
        [ -- toHtml (fittedImage 400 400 "http://piq.codeus.net/static/media/userpics/piq_378272_400x400.png")
          div [style styleCanvas]
              [ toHtml (Collage.collage 500 500 [ model.background, (group formShoots), player, (group formEnemies) ] )
              , Html.text (toString model.pressedKeys)
              -- , Html.text ( toString ( input ) )
              ]


        ]
