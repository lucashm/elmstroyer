module Msg exposing (..)
import Model
import Collage exposing (..)
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
    | Fire Float Bool

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
            isShooting = getShooting
          in
            update ( Fire model.playerPosition ( isShooting model.pressedKeys ) ) {model | playerPosition = newModel}

        KeyboardMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }
              , Cmd.none
            )
            -- let
            --   pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys
            -- in
            --   case pressedKeys of
            --     [ CharR ] ->
            --         update (RotatePlayer (degrees 30)) model
            --
            --     [ CharA ] ->
            --         update (MovePlayerHorizontal -20) model
            --
            --     [ CharD ] ->
            --         update (MovePlayerHorizontal 20) model
            --
            --     [ Space ] ->
            --         update (Fire model.playerPosition) model
            --
            --     _ ->
            --          ( model, Cmd.none )

        Tick newTime ->
          let
            newShoots = List.map (moveY 50) model.shoots
            direction = getDirection model.pressedKeys
          in
            {model
            | time = newTime
            , shoots = newShoots
            }
            |> update (MovePlayerHorizontal direction)
          --  update MoveShoots { model | time = newTime }

        -- MoveShoots ->
        --     let
        --       newShoots = List.map (moveY 20) model.shoots
        --     in
        --       ( {model | shoots = newShoots }, Cmd.none)

        Fire position isShooting ->
            let
                newFire = Collage.move ( position, -160 ) (Collage.filled red (Collage.rect 2 20))
            in
              if isShooting then
                ( { model | shoots = ( append model.shoots [ newFire ] ) }, Cmd.none )
              else
                (model, Cmd.none)

getShooting : List Key -> Bool
getShooting pressedKeys =
    if member Space pressedKeys then
      True
    else
      False


getDirection : List Key -> Float
getDirection pressedKeys =
    if member CharA pressedKeys then
      (-20)
    else if member CharD pressedKeys then
      20
    else
      0
