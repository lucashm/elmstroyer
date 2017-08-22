module Model exposing (..)
import Collage exposing (..)
import Keyboard.Extra exposing (..)
import Color exposing (red, blue)
import Time exposing (..)

type alias Model =
    { player: Collage.Form
    , pressedKeys : List Key
    , playerPosition : Float
    , shots : Collage.Form
    , time : Time
    }


init : Model
init =
     { player = createPlayer
     , pressedKeys = []
     , playerPosition = 0
     , shots = createShot
     , time = 0
     }


createPlayer : Collage.Form
createPlayer =
     Collage.rotate (degrees 90) (Collage.move ( 0, -200 ) (Collage.filled blue (Collage.ngon 3 30)))

createShot : Collage.Form
createShot =
    Collage.filled red (Collage.rect 2 30)
