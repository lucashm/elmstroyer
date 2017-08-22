module View exposing (..)
import Html exposing (Html, text, div, img, canvas, button)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Style exposing (styleMainDiv, styleCanvas)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (black, white, red)
import Model
import Msg



type alias Msg =
    Msg.Msg

type alias Model =
    Model.Model


view : Model.Model -> Html Msg
view model =
    div [style styleMainDiv]
    [ -- toHtml (fittedImage 400 400 "http://piq.codeus.net/static/media/userpics/piq_378272_400x400.png")
      div [style styleCanvas]
          [ toHtml (Collage.collage 500 500 [model.player])
          ]
    , button [ onClick (Msg.RotatePlayer 30 model.player) ] []


    ]




--
-- testing =
--   Collage.collage 500 500 [createBackground, createPlayer]
--
-- createBackground =
--     Collage.filled black (Collage.square 500)
--
-- createPlayer =
--     Collage.filled red (Collage.square 30)
