{-

Functions not found in elm Linear Algebra

-}
module VectorHelpers exposing (..)

import Math.Vector2 exposing (..)
import Random exposing (generate)


origin : Vec2
origin = vec2 0 0


{-|  Vector addition along X axis -}
addX : Float -> Vec2 -> Vec2
addX deltaX vec = setX ((getX vec) + deltaX) vec

{-|  Vector subtraction along X axis -}
subX : Float -> Vec2 -> Vec2
subX deltaX vec = setX ((getX vec) - deltaX) vec

{-|  Vector addition along Y axis -}
addY : Float -> Vec2 -> Vec2
addY deltaY vec = setY ((getY vec) + deltaY) vec

{-|  Vector subtraction along y axis -}
subY : Float -> Vec2 -> Vec2
subY deltaY vec = setY ((getY vec) - deltaY) vec


flipXaxis : Vec2  -> Vec2
flipXaxis v = setX (0 - (getX v)) v

randomVec2 : Float -> Float -> Random.Generator Vec2
randomVec2 min max =
  Random.map2 vec2 (Random.float min max) (Random.float min max)

randomUnitVec2 : Random.Generator Vec2
randomUnitVec2 = randomVec2 1.0 1.0 |> Random.map normalize



-- get angle of a vec2
-- C# code:  get { return Math.Atan2(_y, _x)*(180/Math.PI); }
getVec2Angle : Vec2 -> Float
getVec2Angle v =  atan2 (getY v) (getX v) * (180.0/pi)


vec2FromPolar : Float -> Float -> Vec2
vec2FromPolar radius angle =
    let
      x = cos (degrees angle) * radius
      y = sin (degrees angle) * radius
    in
      vec2 x y

{-| Test to see if a vector is inside a bounding box

-}
vec2IsInBounds : Vec2 -> Float -> Float -> Vec2 -> Bool
vec2IsInBounds origin width height pos =
    let
      x = getX pos
      y = getY pos
      left = getX origin
      bottom = getY origin
      right = left + width
      top = bottom + height
    in
      x >= left && y >= bottom && x < right && y < top
