module Model
    exposing
        (
         initial
        , update
        )

import Bounds

import Levels exposing (..)
import List.Extra exposing (..)
import Math.Vector2 exposing (..)
import Maybe exposing (withDefault)
import Maybe.Extra
import Monocle.Lens exposing (Lens, compose, modify)
import Physics exposing (Physics, PositionedEntity, isCollidingWith)
import Types exposing (..)
import VectorHelpers exposing (..)
import BasicHelpers exposing (..)
import Window
import Time exposing (..)


launchSpeed : Float
launchSpeed =
    0.5


scoreMarkerFloatSpeed : Float
scoreMarkerFloatSpeed =
    0.05

longShotThreshold = 300
ballsPerLevel = 10

physicsLens : Lens { b | physics : Physics } Physics
physicsLens =
    Lens .physics (\p x -> { x | physics = p })


physPositionLens : Lens Physics Vec2
physPositionLens =
    Lens .position (\pos x -> { x | position = pos })


physVelocityLens : Lens { b | velocity : a } a
physVelocityLens =
    Lens .velocity (\v x -> { x | velocity = v })


positionLens : Lens { b | physics : Physics } Vec2
positionLens =
    compose physicsLens physPositionLens


velocityLens : Lens { b | physics : Physics } Vec2
velocityLens =
    compose physicsLens physVelocityLens


resetToLevel : LevelDef -> Model -> Model
resetToLevel ls model =
  { model
    | walls = ls.walls
    , pegs = ls.pegs
    , gameState = Aiming
    , balls = []
    , scoreMarkers = []
    , ballsLeft = model.ballsLeft + ballsPerLevel
  }

initial : Model
initial =
      { score = 0
      , ballsLeft = 0
      , balls = []
      , pegs = []
      , scoreMarkers = []
      , walls = []
      , bucket = { xOffset = 0, width = 100.0, direction = 1.0 }
      , barrelAngle = 45.0
      , gameState = Aiming
      , remainingLevels = Levels.allLevels
      , paused = False
      , windowWidth = 16
      , windowHeight = 16
      } |> resetToLevel initialiseLevel1


--
-- Main Game loop update
--

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of

        WindowResize newSize ->
              getModelWithNewWindowSize model newSize

        Tick deltaTimeMs ->
            if model.paused
                then ( model, Cmd.none)
                else tickModel deltaTimeMs model
        _ ->

          case (model.gameState) of
              Aiming ->
                  case action of

                      UserClicked location ->
                          ( startBallInPlay location model, Cmd.none )

                      MouseMoved location ->
                          ( adjustBarrelAim location model, Cmd.none )

                      _ ->
                          ( model, Cmd.none )

              BallInPlay ->
                  case action of

                      KeyDown keyCode ->
                          case keyCode of
                              65 ->
                                  ( gotoNextLevel model, Cmd.none )

                              80 ->
                                  ({ model | paused = not model.paused } , Cmd.none )

                              _ ->
                                  ( model, Cmd.none )

                      _ ->
                          ( model, Cmd.none )

              SweepingUp ->
                  case action of
                      SweepUp ->
                          removeHitPeg model

                      _ ->
                          ( model, Cmd.none )

              GameOver ->
                      case action of
                          PlayAgain ->
                              ( { initial | windowWidth = model.windowWidth, windowHeight = model.windowHeight } ,
                                     Cmd.none)

                          _ ->
                              ( model, Cmd.none )



gravity : Vec2
gravity =
    vec2 0.0 0.001





adjustBarrelAim : Vec2 -> Model -> Model
adjustBarrelAim mousePosition model =
    let
        gunpos =
            vec2 (Bounds.gameX / 2) 0.0

        barrelAngle_ =
            sub mousePosition gunpos |> getVec2Angle
    in
        { model | barrelAngle = barrelAngle_ }


initialBall : Ball
initialBall =
    { physics =
        { mass = 10.0
        , radius = 5.0
        , position = vec2 (Bounds.gameX / 2) 10.0
        , velocity = vec2 0.0 0.0
        , acceleration = vec2 0.0 0.0
        }
      , hitCount = 0
      , lastHitLocation = vec2 0.0 0.0
    }


startBallInPlay : Vec2 -> Model -> Model
startBallInPlay clickedPosition model =
    let
        gunpos =
            vec2 (Bounds.gameX / 2) 0.0

        launchVector =
            vec2FromPolar launchSpeed model.barrelAngle

        barrelTip =
            vec2FromPolar 40.0 model.barrelAngle |> add gunpos

        launchedBall =
            initialBall
                |> velocityLens.set launchVector
                |> positionLens.set barrelTip
                |> (\b -> {b | lastHitLocation = barrelTip})
    in
        { model
          | gameState = BallInPlay
          , balls = [ launchedBall ]
        }


gotoNextLevel : Model -> Model
gotoNextLevel model =
    case model.remainingLevels of
        nextLevel :: levelsRemaining ->
            { model | remainingLevels = levelsRemaining } |> resetToLevel nextLevel

        [] ->
            -- HACK - run out of levels
            model |> resetToLevel initialiseLevel1


{-| Called whhen all the hit pegs have been swept
-}
endSweep : Model -> Model
endSweep model =
    if not (List.any (\p -> p.pegType == Red) model.pegs) then
        gotoNextLevel model
    else
        { model
            | gameState =
                if (model.ballsLeft > 1) then
                    Aiming
                else
                    GameOver
                --, levelState = levelStatBallLens.set [initialBall] model.levelState
            , ballsLeft = model.ballsLeft - 1
            , balls = []
        }


removeHitPeg : Model -> ( Model, Cmd Msg )
removeHitPeg model =
        model.pegs
            |> List.filter (\p -> p.hitCount > 0)
            |> List.head
            |> Maybe.map (\p -> ( { model | pegs = model.pegs |> remove p }, doAfterDelay SweepUp 100))
            |> withDefault ( endSweep model, Cmd.none )




tickModel : Time -> Model -> ( Model, Cmd Msg )
tickModel deltaTimeMs model =
    let
        model_  = model
            |> (tickBalls deltaTimeMs)
            |> (tickPegs deltaTimeMs)
            |> processWallCollisions
            |> (processScoreMarkers deltaTimeMs)
            |> (tickBucket deltaTimeMs)
            |> processPegCollisions
            |> processBucketCollisions

        isBallsAtBottom = model_.balls
                        |> List.map (positionLens.get >> getY)
                        |> List.all ((<) Bounds.gameY)

        ( newState, command ) =
            if model_.gameState == BallInPlay && isBallsAtBottom then
                ( SweepingUp, doAfterDelay SweepUp 100 )
            else
                ( model_.gameState, Cmd.none )
    in
        (   { model_  | gameState = newState } , command )




tickPegs : Time -> Model -> Model
tickPegs deltaTimeMs model =
    let
        tickPeg peg =
            { peg | scoreDisplayTimeLeft = max (peg.scoreDisplayTimeLeft - deltaTimeMs) 0 }

    in
        { model | pegs = List.map tickPeg model.pegs }

tickBalls : Time -> Model -> Model
tickBalls deltaTimeMs model =
    let
        tickBall ball =
            ball
                |> modify physicsLens (\p -> { p | acceleration = gravity })
                |> modify physicsLens (Physics.tickPhysics deltaTimeMs)

    in
        { model | balls = model.balls |> List.map tickBall }


tickBucket : Float -> Model -> Model
tickBucket deltaTimeMs model =
    let
        updateBucket bucket =
            { bucket
                | xOffset = bucket.xOffset + bucket.direction
                , direction =
                    if (bucket.xOffset == 0) then
                        1.0
                    else if (bucket.xOffset == (Bounds.gameX - bucket.width)) then
                        -1.0
                    else
                        bucket.direction
            }
    in
        { model | bucket = updateBucket model.bucket }


processScoreMarkers : Float -> Model -> Model
processScoreMarkers deltaTimeMs model =
    let
        tickMarker marker =
            { marker
                | opacity = marker.opacity - (marker.scoreMarkerFadeSpeed * deltaTimeMs)
                , position = marker.position |> subY (scoreMarkerFloatSpeed * deltaTimeMs)
            }

        scoreMarkers_ =
            model.scoreMarkers |> List.map tickMarker |> List.filter (\sm -> sm.opacity > 0.0)
    in
        { model | scoreMarkers = scoreMarkers_ }



{- Process collisions between ball and walls.    Walls are only vertical at the moment so the rebound is only reflected in the H axis -}
processWallCollisions : Model -> Model
processWallCollisions model =
    let
        isCollidingWithWall : PositionedEntity a -> VerticalWall -> Bool
        isCollidingWithWall entity wall =
            (abs ((getX entity.position) - wall.hPos)) < (entity.radius + (wall.width / 2))

        testCollideWithWall : Ball -> Model -> Model
        testCollideWithWall ball modelSoFar =
            model.walls
                |> List.filter (isCollidingWithWall ball.physics)
                |> List.head
                |> Maybe.map (collideWithWall modelSoFar ball)
                |> Maybe.withDefault modelSoFar
    in
        model.balls |> List.foldl testCollideWithWall model



-- TODO - consider the end caps


collideWithWall : Model -> Ball -> VerticalWall -> Model
collideWithWall model ball wall =
    let
        offset =
            if (getX ball.physics.position) > wall.hPos then
                (wall.width + ball.physics.radius)
            else
                0 - (wall.width + ball.physics.radius)

        ball_ =
            ball
                |> modify positionLens (setX (wall.hPos + offset))
                |> modify velocityLens flipXaxis
    in
        { model | balls =  ball_ :: (remove ball model.balls) }

processBucketCollisions :  Model ->  Model
processBucketCollisions model =
  let
      -- For each ball that is touching the bucket:
      --  * Add a bonus ball
      --  * Create bonus popup
      --  * Remove that ball from play

    isInBucket ball  =  (getX ball.physics.position) > model.bucket.xOffset + ball.physics.radius
                            && (getX ball.physics.position) < (model.bucket.xOffset + model.bucket.width - ball.physics.radius)
                            && (getY ball.physics.position) > Bounds.gameY

    ballsInBucket = model.balls |> List.filter isInBucket

    popups = ballsInBucket |> List.map  (\b -> newScoreMarker Nothing "FREE BALL"  (setY Bounds.gameY b.physics.position))

  in
    { model
          | balls = model.balls |> List.filter (\b -> List.Extra.notMember b ballsInBucket)
          , scoreMarkers = List.concat  [popups, model.scoreMarkers ]
          , ballsLeft = model.ballsLeft + (List.length ballsInBucket)
          }


{- Process collisions between ball and other pegs. if a collision is found,
   then reverse the velocity on by the normal of the collision surface
   and add some points
-}
processPegCollisions : Model -> Model
processPegCollisions model =
    let
        testCollide : Ball -> Model -> Model
        testCollide ball modelSoFar =
            model.pegs
                |> List.filter (isCollidingWith ball.physics)
                |> List.head
                |> Maybe.map (collideWithPeg modelSoFar ball)
                |> Maybe.withDefault modelSoFar
    in
       model.balls |> List.foldl testCollide model






collideWithPeg : Model -> Ball -> Peg -> Model
collideWithPeg model ball peg =
    let
        (pointsEarned, maybeBonus) =
              case peg.pegType of
                  MultiBall -> (100,  if peg.hitCount == 0 then Just "MULTI BALL" else Nothing)
                  Red -> (50,
                    (case  peg.hitCount of
                      2 -> Just "TRIPLE HIT"
                      3 -> Just "QUADRUPLE HIT"
                      _ ->  Nothing))
                  Normal ->
                      case  peg.hitCount of
                        2 -> (40, Just "TRIPLE HIT")
                        3 -> (40, Just "QUADRUPLE HIT")
                        count -> (10 * (count + 1), Nothing)

        peg_ =
            { peg
              | hitCount = peg.hitCount + 1
              , scoreLastHit = pointsEarned
              , scoreDisplayTimeLeft = 600
             }

        pegs_ =
            model.pegs |> replaceIf (\p -> p == peg) peg_


        physUpdate physics =
            let
                -- Co-erce the position so that the ball and peg do not overlap (TODO -  backout on velocity vector)
                newBallPos =
                    tweakPositionToSurface physics peg

                -- switch out the velocity so that it is away from the hit peg
                -- this cant be done with  just a negate of the velocity Vector
                bounceV =
                    sub newBallPos peg.position |> normalize |> scale (length physics.velocity)
            in
                { physics | velocity = bounceV, position = newBallPos }


        farShotBonus = Just (Math.Vector2.distance  peg.position  ball.lastHitLocation)
                |> Maybe.Extra.filter (\d -> d > longShotThreshold)
                |> Maybe.map (\d -> newScoreMarker Nothing "LONG SHOT" peg.position)


        temp =
            { ball
                | hitCount = ball.hitCount + 1
                , lastHitLocation = peg.position
            }

        reboundedBall = modify physicsLens physUpdate temp


        socreMarkers_ =  Maybe.Extra.values [ (maybeBonus |> Maybe.map (\b -> newScoreMarker (Just pointsEarned) b peg.position)), farShotBonus ]
                           |> List.append model.scoreMarkers

        socreMarkers__ = if reboundedBall.hitCount >= 8
          then (newScoreMarker Nothing  (toString  reboundedBall.hitCount ++ " HITS") peg.position) :: socreMarkers_
          else socreMarkers_

        balls_ =
          (case peg.pegType of
              MultiBall ->  if (peg.hitCount == 0) then (reboundedBall |>  modify velocityLens flipXaxis )  ::  model.balls else model.balls
              _ -> model.balls)
                |>  replaceIf (\b -> b == ball) reboundedBall

    in
            { model
                | pegs = pegs_
                , balls = balls_
                , scoreMarkers = socreMarkers__
                , score = model.score + pointsEarned
            }



-- TODO: backout via velocity vector


tweakPositionToSurface : PositionedEntity a -> Peg -> Vec2
tweakPositionToSurface ball peg =
    sub ball.position peg.position
        |> normalize
        |> scale (ball.radius + peg.radius)
        |> add peg.position


getModelWithNewWindowSize : Model -> Window.Size -> ( Model, Cmd Msg )
getModelWithNewWindowSize model windowSize =
    let
        model_ =
            { model
                | windowWidth = windowSize.width
                , windowHeight = windowSize.height
            }
    in
        ( model_, Cmd.none )


newScoreMarker : (Maybe Int) -> String -> Vec2 -> ScoreMarker
newScoreMarker score text pos =
    { radius = 22
    , position = pos
    , text =  text ++ (score |> Maybe.map (\s -> "\n" ++ (toString s)) |> withDefault "")
    , opacity = 0.5
    , scoreMarkerFadeSpeed = 0.5 / 2000.0
    }
