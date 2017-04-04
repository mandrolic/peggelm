module Bounds exposing (..)


gameX : Float
gameX =
    500.0

gameY : Float
gameY =
    500.0

--centerHoriz : Float -> Float



centerHoriz width = ((gameX - width) /2)

centerVert : Float -> Float
centerVert height = ((gameY - height) /2)
