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
    | MoveShoots
    | SpinToWin Bool

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
                      (-5) ->
                        update (UpdatePlayerPosition deslocation) {model | player = newModel}

                      _ ->
                        (model, Cmd.none)

              (-220) ->
                  case deslocation of
                      (-5) ->
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
            {model | playerPosition = newModel} ! []


        KeyboardMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }
              , Cmd.none
            )

        -- update function, run 30 times per second.
        Tick newTime ->
          let
            direction = getDirection model.pressedKeys
            isShooting = getShooting model.pressedKeys
            isSpinning = getSpinning model.pressedKeys
          in
            update (MovePlayerHorizontal direction) { model | time = newTime }
              |> andThen (MovePlayerHorizontal direction)
              |> andThen (Fire model.playerPosition isShooting )
              |> andThen MoveShoots
              |> andThen (SpinToWin isSpinning)


        MoveShoots ->
            let
              newShoots = List.map (moveY 25) model.shoots
            in
              ( {model | shoots = newShoots }, Cmd.none)


        Fire position isShooting ->
            let
                newFire = Collage.move ( position, -190 ) (Collage.filled red (Collage.rect 3 15))
            in
              if isShooting then
                ( { model | shoots = ( append model.shoots [ newFire ] ) }, Cmd.none )
              else
                (model, Cmd.none)

        -- useless but fun
        SpinToWin isSpinning ->
            if isSpinning then
              {model | player = Collage.rotate (degrees 30) model.player} ! []
            else
              (model, Cmd.none)



getShooting : List Key -> Bool
getShooting pressedKeys =
    if member Space pressedKeys then
      True
    else
      False

getSpinning : List Key -> Bool
getSpinning pressedKeys =
    if member CharR pressedKeys then
      True
    else
      False


getDirection : List Key -> Float
getDirection pressedKeys =
    if member CharA pressedKeys then
      (-5)
    else if member CharD pressedKeys then
      5
    else
      0

andThen : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen msg ( model, cmd ) =
    let
        ( newmodel, newcmd ) =
            update msg model
    in
        newmodel ! [ cmd, newcmd ]
