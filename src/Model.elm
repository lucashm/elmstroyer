module Model exposing (..)
import Collage exposing (..)
import Color exposing (red, blue)

type alias Model =
    {player: Collage.Form}


init : Model
init =
     {player = createPlayer}


createPlayer : Collage.Form
createPlayer =
     Collage.filled blue (Collage.ngon 5 30)
