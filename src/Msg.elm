module Msg exposing (..)
import Model
import Collage exposing (rotate)


type alias Model =
    Model.Model


type Msg
    = RotatePlayer Float Collage.Form


update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    case msg of
        RotatePlayer degree object ->
          let
            newModel = rotate degree model.player
          in
            ( {model | player = newModel}, Cmd.none )
