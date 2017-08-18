module Msg exposing (..)
import Model


type alias Model =
    Model.Model


type Msg
    = NoOp


update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
