module Physics exposing (..)

import Math.Vector2 exposing (..)
import VectorHelpers exposing (..)

type alias PositionedEntity a = { a |
  position : Vec2
  , radius : Float
}

type alias Physics = PositionedEntity ({
     mass : Float
    , velocity : Vec2
    , acceleration : Vec2
  })


gravitationalConstant : Float
gravitationalConstant = 100.0

fluidMassDensity : Float
fluidMassDensity = 0.0015



divide : Vec2 -> Float -> Vec2
divide v q = scale (1/q) v

circleArea : Float -> Float
circleArea radius = radius^2 * pi


{-| Generic Physics dt Ticck - deals with acc, vel, drag etc for all 'PhysicsMotion' types  -}
tickPhysics :  Float -> Physics -> Physics
tickPhysics dt thing =
    let
        dragForce = getDragDecellerationForce dt thing
        dragDecceleration = divide dragForce thing.mass
        dragAddjustedAcc = add thing.acceleration dragDecceleration
        velocity_ = add thing.velocity  (scale dt dragAddjustedAcc)
        displacement =  (scale dt velocity_)
        position_ = add thing.position  displacement
    in
        { thing
            | position = position_
            , velocity = velocity_
            , acceleration = origin                         -- Reset acceleration for this dt; it will be calculated again next time
        }



 -- Gets the drag force of a sphere
getDragForce : Float -> Vec2 -> Vec2
getDragForce radius velocity  =
    {-
    Drag force - http://en.wikipedia.org/wiki/Drag_equation
    F = 0.5p(V^2)CdA

    Where Cd for a sphere is 0.4 http://en.wikipedia.org/wiki/Drag_coefficient
    p is mass density of the fluid
    -}
    if (length velocity == 0)
    then
        velocity
    else
        let
            scalarForce = 0.5 * fluidMassDensity * (lengthSquared velocity) * 0.4 * circleArea(radius)
        in
            velocity |> normalize |>  Math.Vector2.negate  |> scale scalarForce


getDragDecellerationForce : Float -> Physics  -> Vec2
getDragDecellerationForce dt thing =
        scale dt thing.acceleration         -- new velocity delta
            |> add thing.velocity           -- add to existing
            |> getDragForce thing.radius    -- from that velocity calc the drag force Vector2



isCollidingWith : PositionedEntity a -> PositionedEntity b -> Bool
isCollidingWith p q =
    (length (sub p.position q.position)) < (p.radius + q.radius)
