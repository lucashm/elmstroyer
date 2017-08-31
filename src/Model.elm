module Model exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard.Extra exposing (..)
import Time exposing (..)
import Collision2D exposing (..)

type alias Model =
    { player: (Collage.Form, Rectangle)
    , pressedKeys : List Key
    , playerPosition : Float
    , shoots : List ((Collage.Form, Rectangle), (Float, Float))
    , time : Time
    , enemies : List ((Collage.Form, Rectangle), (Float, Float))
    , randomNumber : Int
    , background : Collage.Form
    }


init : Model
init =
     { player = (createPlayer, createPlayerCollision)
     , pressedKeys = []
     , playerPosition = 0
     , shoots = []
     , time = 0
     , enemies = []
     , randomNumber = 0
     , background = createBackground
     }


createPlayerCollision : Rectangle
createPlayerCollision =
    rectangle 0 -200 41 34

createPlayer : Collage.Form
createPlayer =
     fittedImage 41 34 "https://cdn.discordapp.com/attachments/204397763637673984/351825513620439042/navezinha2.gif"
      |> toForm
      |> Collage.move (0, -200)



createBackground : Collage.Form
createBackground =
    fittedImage 500 500 "https://cdn.discordapp.com/attachments/204397763637673984/351843584347078656/estrelinhas.gif"
      |> toForm
