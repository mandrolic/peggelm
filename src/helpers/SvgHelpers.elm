{-

Functions not found in elm Basic libs

-}
module SvgHelpers exposing (..)

import Svg exposing (Svg, circle, rect, svg, tspan)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Types exposing (..)
import Math.Vector2 exposing (..)
import VectorHelpers exposing (..)



type SvgTransforms
    = Rotate Float Float Float
      -- rotate in degrees
    | Translate Float Float
    | Scale Float


tostr : SvgTransforms -> String
tostr tfm =
    case tfm of
        Rotate degree x y ->
            "rotate(" ++ toString degree ++ " " ++ toString x ++ " " ++ toString y ++ ")"

        Translate x y ->
            "translate(" ++ toString x ++ " " ++ toString y ++ ")"

        Scale scale ->
            "scale(" ++ (toString scale) ++ ")"


tfm : List SvgTransforms -> String
tfm transforms =
    String.join "," (List.map tostr transforms)


translateVec2 : Vec2 -> SvgTransforms
translateVec2 pos =
    Translate (getX pos) (getY pos)
