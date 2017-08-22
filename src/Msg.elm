module Msg exposing (..)
import Model
import Collage exposing (..)
import Keyboard.Extra exposing (Key(..))
import Time exposing (..)
import List exposing (..)


type alias Model =
    Model.Model


type Msg
    = RotatePlayer Float
    | KeyboardMsg Keyboard.Extra.Msg
    | MovePlayerHorizontal Float
    | UpdatePlayerPosition Float
    | Tick Time
    | MoveShots

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

                _ ->
                      ( model, Cmd.none )

        Tick newTime ->
            update MoveShots { model | time = newTime }

        MoveShots ->
            let
              newShots = (moveY 20 model.shots)
            in
              ({model | shots = newShots}, Cmd.none)
