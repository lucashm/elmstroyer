module Msg exposing (..)
import Model
import Collage exposing (..)
import Keyboard.Extra exposing (Key(..))
import Time exposing (..)
import List exposing (..)
import Color exposing (red, purple)
import Random
import Collision2D

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
    | MoveEnemies
    | SpinToWin Bool
    | DestroyFire
    | Spawn Time
    | ShootTimer Time
    | RollRandom
    | NewRandom Int
    | DestroyEnemy
--    | HitEnemy Time

update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    case msg of

        RollRandom ->
          (model, Random.generate NewRandom (Random.int (-220) 220))

        NewRandom number ->
          ({ model | randomNumber = number }, Cmd.none)

        RotatePlayer degree ->
          let
            (player, collision) = model.player
            newModel = rotate degree player
          in
            ( {model | player = (newModel, collision)}, Cmd.none )


        MovePlayerHorizontal deslocation ->
          let
            (player, collision) = model.player
            newModel = moveX deslocation player
          in
          -- Needed to limit player movement
            case model.playerPosition of
              220 ->
                  case deslocation of
                      (-5) ->
                        update (UpdatePlayerPosition deslocation) {model | player = (newModel, collision)}

                      _ ->
                        (model, Cmd.none)

              (-220) ->
                  case deslocation of
                      (-5) ->
                        (model, Cmd.none)

                      _ ->
                        update (UpdatePlayerPosition deslocation) {model | player = (newModel, collision)}


              _ ->
                  update (UpdatePlayerPosition deslocation) {model | player = (newModel, collision)}


        UpdatePlayerPosition deslocation ->
          let
            newModel = model.playerPosition + deslocation
            (player, collision) = model.player
            newCollision = Collision2D.rectangle newModel 0 41 34
            isShooting = getShooting
          in
            {model | playerPosition = newModel, player = (player, newCollision)} ! []


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
            --  |> andThen (Fire model.playerPosition isShooting )
              |> andThen MoveShoots
              |> andThen DestroyFire
              |> andThen MoveEnemies
              |> andThen DestroyEnemy
              |> andThen (SpinToWin isSpinning)


        ShootTimer newTime ->
          let
            isShooting = getShooting model.pressedKeys
          in
            update (Fire model.playerPosition isShooting ) {model | time = newTime}


        Spawn newTime ->
          let
            newEnemy = Collage.square 40
                        |> Collage.filled purple
                        |> Collage.move ( (toFloat model.randomNumber) , 220 )
            newEnemyPosition = ( (toFloat model.randomNumber), 220)
            newCollision = Collision2D.rectangle (toFloat model.randomNumber) 220 40 40

          in
            {model
            | time = newTime
            , enemies = append model.enemies [ ( (newEnemy, newCollision), newEnemyPosition ) ]
            } ! []
            |> andThen RollRandom


        MoveEnemies ->
            let
              newEnemies = List.map (moveY (-5)) (separeForm model.enemies)

              axis = separeFormCoord model.enemies
                    |> map updateEnemyPosition

              newEnemiesCollision = List.map (moveCollision -5 40 40) (separeFormCoord model.enemies)

            in
              ({model | enemies = zip (zip newEnemies newEnemiesCollision) axis}, Cmd.none)

        MoveShoots ->
            let
              newShoots = List.map (moveY 25) (separeForm model.shoots)
              axis =
                  separeFormCoord model.shoots
                  |> map updateShootPosition

              newShootsCollision = List.map (moveCollision 25 3 15) (separeFormCoord model.shoots)

            in
              ( {model
                | shoots =  zip (zip newShoots newShootsCollision) axis
                } , Cmd.none)


        Fire position isShooting ->
            let
                newFire = Collage.move ( position, -190 ) (Collage.filled red (Collage.rect 3 15))
                newFirePosition = (position, -190)
                newFireCollision = Collision2D.rectangle position -190 3 15
            in
              if isShooting then
                ( { model
                  | shoots = ( append model.shoots [((newFire, newFireCollision), (position, -190))] )
                  }, Cmd.none )
              else
                (model, Cmd.none)


        DestroyFire ->
            let
              getShoot =
                    filter filterShoots model.shoots
            in
              ({ model
               | shoots = getShoot
               }, Cmd.none)

        DestroyEnemy ->
            let
              getEnemy =
                    filter filterEnemies model.enemies
            in
              ({ model
               | enemies = getEnemy
               }, Cmd.none)



        -- HitEnemy newTime ->
        --   let
        --     newEnemies = ifEnemyHit model.enemies model.shoots
        --   in
        --     { model | enemies = newEnemies, time = newTime } ! []

        -- useless but fun
        SpinToWin isSpinning ->
          let
            (player, collision) = model.player
          in
            if isSpinning then
              {model | player = (Collage.rotate (degrees 30) player, collision)} ! []
            else
              (model, Cmd.none)




moveCollision : Float -> Float -> Float -> (Float, Float) -> Collision2D.Rectangle
moveCollision moving width_ height_  axis =
  let
    (x,y) = axis
    newCollision = Collision2D.rectangle  x (y + moving) width_ height_
  in
    newCollision

-- ifEnemyHit listEnemy listFire =
--   filter (isHit listFire) listEnemy


isHit listFire (enemy, (x,y)) =
  let
    coordFire = separeFormCoord listFire
  in
    if member (x,y) coordFire then
      False
    else
      True

filterEnemies : ( (Form, Collision2D.Rectangle), (Float, Float) ) -> Bool
filterEnemies (enemy, (x,y)) =
  if y < -500 then
    False
  else
    True


filterShoots : ( (Form, Collision2D.Rectangle), (Float, Float) ) -> Bool
filterShoots (shoot, (x,y)) =
  if y > 500 then
    False
  else
    True





updateShootPosition : (Float, Float) -> (Float, Float)
updateShootPosition (x , y) =
  (x, y + 25)


updateEnemyPosition : (Float, Float) -> (Float, Float)
updateEnemyPosition (x, y) =
  (x, y - 5)


zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xBack, y :: yBack ) ->
        (x,y) :: zip xBack yBack

    (_, _) ->
        []



separeForm : List ( (Form, Collision2D.Rectangle) , (Float, Float) ) -> List Form
separeForm list =
    let
      (a,b) = unzip list
      (form_, collision) = unzip a
    in
      form_

separeFormCoord : List ( (Form, Collision2D.Rectangle), (Float, Float) ) -> List (Float, Float)
separeFormCoord list =
    let
      (a,b) = unzip list
    in
      b

separeCollision : List ( (Form, Collision2D.Rectangle) , (Float, Float) ) -> List Collision2D.Rectangle
separeCollision list =
    let
      (a,b) = unzip list
      (form_, collision) = unzip a
    in
      collision


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
