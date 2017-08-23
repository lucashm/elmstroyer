module Model exposing (..)
import Collage exposing (..)
import Keyboard.Extra exposing (..)
import Color exposing (red, blue, purple)
import Time exposing (..)

type alias Model =
    { player: Collage.Form
    , pressedKeys : List Key
    , playerPosition : Float
    , shoots : List (Collage.Form, (Float, Float))
    , time : Time
    , enemies : List (Collage.Form, (Float, Float))
    , randomNumber : Int
    }


init : Model
init =
     { player = createPlayer
     , pressedKeys = []
     , playerPosition = 0
     , shoots = [ (anotherShoot, (0,-200)), (createShoot, (0,0)) ]
     , time = 0
     , enemies = [(createEnemy, (0,0))]
     , randomNumber = 0
     }

createEnemy : Collage.Form
createEnemy =
      Collage.rotate (degrees 90) (Collage.move ( 0, 220 ) (Collage.filled purple (Collage.ngon 4 30)))


createPlayer : Collage.Form
createPlayer =
     Collage.rotate (degrees 90) (Collage.move ( 0, -200 ) (Collage.filled blue (Collage.ngon 3 30)))

createShoot : Collage.Form
createShoot =
    Collage.filled red (Collage.rect 2 30)

anotherShoot : Collage.Form
anotherShoot =
    Collage.move ( 0, -200 ) (Collage.filled red (Collage.rect 2 30))
