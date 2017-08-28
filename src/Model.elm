module Model exposing (..)
import Collage exposing (..)
import Element exposing (..)
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
    , background : Collage.Form
    }


init : Model
init =
     { player = createPlayer
     , pressedKeys = []
     , playerPosition = 0
     , shoots = []
     , time = 0
     , enemies = []
     , randomNumber = 0
     , background = createBackground
     }

createPlayer : Collage.Form
createPlayer =
     fittedImage 41 34 "https://cdn.discordapp.com/attachments/204397763637673984/351483067153645569/navezinha.gif"
      |> toForm
      |> Collage.move (0, -200)



createBackground : Collage.Form
createBackground =
    fittedImage 500 500 "https://media.giphy.com/media/ZiDpJRogbB9V6/giphy.gif"
      |> toForm
