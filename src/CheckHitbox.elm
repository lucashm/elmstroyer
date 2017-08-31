module CheckHitbox exposing (..)
import Msg exposing (..)
import List exposing (..)

isHit listFire (enemy, (x,y)) =
  let
    coordFire = separeFormCoord listFire
  in
    if member (x,y) coordFire then
      False
    else
      True
