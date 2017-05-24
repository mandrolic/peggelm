module Types exposing (..)

import Keyboard exposing (KeyCode)
import Time exposing (..)
import Physics
import Window
import Math.Vector2 exposing (..)

type alias Ball =
    { physics : Physics.Physics
     , hitCount : Int
     , lastHitLocation : Vec2
    }


type PegType = Normal | Red | MultiBall

type alias Peg =
    Physics.PositionedEntity {
    pegType : PegType
    , hitCount : Int
    , scoreLastHit : Int
    , scoreDisplayTimeLeft : Float
  }


type alias VerticalWall =
    { lowerBound : Float
    , upperBound : Float
    , hPos : Float
    , width : Float
    }

type alias ScoreMarker =
    Physics.PositionedEntity {
    text : String
    , opacity : Float
    , scoreMarkerFadeSpeed : Float
  }

type alias Bucket = {
    xOffset : Float
    , width : Float
    , direction : Float
}


type alias LevelDef =
    {
     pegs : List Peg
    , walls : List VerticalWall
    }




type Msg
    = Tick Time
    | UserClicked Vec2
    | MouseMoved Vec2
    | SweepUp
    | WindowResize Window.Size
    | KeyDown KeyCode
    | PlayAgain


type GameState
    = Aiming
    | BallInPlay
    | SweepingUp
    | GameOver

type BarrelMoveDirection = Right | Left



type alias Model =
    { gameState : GameState
    , barrelAngle : Float
    , barrelMoveDirection : BarrelMoveDirection
    , score : Int
    , ballsLeft : Int
    , remainingLevels : List LevelDef
    , paused : Bool
    , balls : List Ball
    , pegs : List Peg
    , redPegTargetForCurrentLevel : Int
    , walls : List VerticalWall
    , scoreMarkers : List ScoreMarker
    , bucket : Bucket
    , windowWidth : Int
    , windowHeight : Int
    }
