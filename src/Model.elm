module Model exposing (..)
import Collage exposing (..)
import Keyboard.Extra exposing (..)
import Color exposing (red, blue)

type alias Model =
    { player: Collage.Form
    , pressedKeys : List Key
    , playerPosition : Float
    }


init : Model
init =
     { player = createPlayer
     , pressedKeys = []
     , playerPosition = 0
     }


createPlayer : Collage.Form
createPlayer =
     Collage.rotate (degrees 90) (Collage.move ( 0, -200 ) (Collage.filled blue (Collage.ngon 3 30)))
