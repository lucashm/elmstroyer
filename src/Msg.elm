module Msg exposing (..)
import Model
import Collage exposing (..)
import Element exposing (..)
import Keyboard.Extra exposing (Key(..))
import Time exposing (..)
import List exposing (..)
import Color exposing (red)


type alias Model =
    Model.Model


type Msg
    = RotatePlayer Float
    | KeyboardMsg Keyboard.Extra.Msg
    | MovePlayerHorizontal Float
    | UpdatePlayerPosition Float
    | Tick Time
    | MoveShoots
    | Fire Float

update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    case msg of
        RotatePlayer degree ->
          let
            newModel = rotate degree model.player
          in
            ( {model | player = newModel}, Cmd.none )


        MovePlayerHorizontal deslocation ->
          let
            newModel = moveX deslocation model.player
          in
          -- Needed to limit player movement
            case model.playerPosition of
              220 ->
                  case deslocation of
                      (-20) ->
                        update (UpdatePlayerPosition deslocation) {model | player = newModel}

                      _ ->
                        (model, Cmd.none)

              (-220) ->
                  case deslocation of
                      (-20) ->
                        (model, Cmd.none)

                      _ ->
                        update (UpdatePlayerPosition deslocation) {model | player = newModel}


              _ ->
                  update (UpdatePlayerPosition deslocation) {model | player = newModel}


        UpdatePlayerPosition deslocation ->
          let
            newModel = model.playerPosition + deslocation
          in
          ({model | playerPosition = newModel}, Cmd.none)

        KeyboardMsg keyMsg ->
            let
              pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys
            in
              case pressedKeys of
                [ CharR ] ->
                    update (RotatePlayer (degrees 30)) model

                [ CharA ] ->
                    update (MovePlayerHorizontal -20) model

                [ CharD ] ->
                    update (MovePlayerHorizontal 20) model

                [ Space ] ->
                    update (Fire model.playerPosition) model

                _ ->
                      ( model, Cmd.none )

        Tick newTime ->
            update MoveShoots { model | time = newTime }

        MoveShoots ->
            let
              newShoots = map (moveY 20) model.shoots
            in
              ( {model | shoots = newShoots }, Cmd.none)

        Fire position ->
            let
                newFire = Collage.move ( position, -200 ) (Collage.filled red (Collage.rect 2 30))
            in
              ( { model | shoots = ( append model.shoots [ newFire ] ) }, Cmd.none )
