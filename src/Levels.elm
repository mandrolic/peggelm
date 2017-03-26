module Levels exposing (allLevels, initialiseLevel1)

import Bounds
import Math.Vector2 exposing (..)
import Random
import Types exposing (..)
import VectorHelpers exposing (..)


addPegX : Float -> { a | position : Vec2 } -> { a | position : Vec2 }
addPegX offset peg =
    { peg | position = (addX offset peg.position) }

addPegXs : Float -> List Peg -> List Peg
addPegXs  =
    List.map << addPegX


addPegY : Float -> { a | position : Vec2 } -> { a | position : Vec2 }
addPegY offset peg =
    { peg | position = (addY offset peg.position) }

addPegYs : Float -> List { a | position : Vec2 } -> List { a | position : Vec2 }
addPegYs  =
    addPegY >> List.map

sideWalls : List VerticalWall
sideWalls =
    [ { upperBound = 0, lowerBound = Bounds.gameY, hPos = 0, width = 4 }
    , { upperBound = 0, lowerBound = Bounds.gameY, hPos = Bounds.gameX - 4, width = 4 }
    ]


initialPeg : Float -> Float -> Peg
initialPeg x y =
    { position = vec2 x y
    , radius = 6
    , hitCount = 0
    , pegType = Normal
    , scoreLastHit = 0
    , scoreDisplayTimeLeft = 0.0
    }


setRed : { b | pegType : a } -> { b | pegType : PegType }
setRed p =
    { p | pegType = Red }

setRedIf : (a -> Bool) -> a -> Peg -> Peg
setRedIf testFunc val peg =
    if (testFunc val)
      then setRed peg
      else peg


circlePegs : Float -> Float -> Float -> Int -> List Peg
circlePegs x y radius count =
    let
        radiansPerPeg =
            2 * pi / (toFloat count)
    in
        List.range 0 (count - 1)
            |> List.map (\pegNo -> (toFloat pegNo) * radiansPerPeg)
            |> List.map (\angle -> ( x + (cos angle * radius), y + (sin angle * radius) ))
            |> List.map (\( l, t ) -> initialPeg l t)


hLineOffPegs : Int -> Float -> List Peg
hLineOffPegs count spacing =
    List.range 0 (count - 1)
        |> List.map toFloat
        |> List.map ((*) spacing)
        |> List.map (\x -> initialPeg x 0)



initialiseLevel1 : LevelDef
initialiseLevel1 =
  let
    lineOfPegs = hLineOffPegs 8 50
  in
    { pegs =
        List.concat
            [ lineOfPegs |> addPegXs 50 |> addPegYs 150
            , lineOfPegs |> addPegXs 70 |> addPegYs 200.0 |> List.map setRed
            , lineOfPegs |> addPegXs 50 |> addPegYs 250.0
            , lineOfPegs |> addPegXs 70 |> addPegYs 300.0 |> List.map setRed
            , lineOfPegs |> addPegXs 50 |> addPegYs 350.0
            ]
    , walls = sideWalls
    }


circleMultiBallLevel : LevelDef
circleMultiBallLevel =
    let
        makeRedIfEven = setRedIf (\i -> i % 2 == 0)
    in
        { pegs =
            List.concat
                [ circlePegs (Bounds.gameX / 2) 200.0 75.0 16 |> List.indexedMap makeRedIfEven
                , [ initialPeg (Bounds.gameX / 2) 200 ] |> List.map (\peg -> { peg | pegType = MultiBall })
                , hLineOffPegs 9 40 |> addPegYs 320.0
                , hLineOffPegs 8 40 |> addPegXs 20.0 |> addPegYs 360.0 |> List.map setRed
                , hLineOffPegs 9 40 |> addPegYs 400.0
                ]
        , walls = sideWalls
        }


randomRedPegs : List Peg -> List Peg
randomRedPegs pegs =
  let
      pegGenerator =
          Random.float 0 1
              |> Random.map
                  (\f ->
                      if (f > 0.3) then
                          Red
                      else
                          Normal
                  )
  in
    pegs |> List.indexedMap (\pos p -> { p | pegType = Random.step pegGenerator (Random.initialSeed pos) |> Tuple.first })


bewbsLevel : LevelDef
bewbsLevel =
    let
        makeRedIfEven = setRedIf (\i -> i % 2 == 0)
    in
        { pegs =
            List.concat
                [ circlePegs 150.0 200.0 75.0 16 |> List.indexedMap makeRedIfEven
                , circlePegs 350.0 200.0 75.0 16 |> List.indexedMap makeRedIfEven
                , [ initialPeg 150.0 200, initialPeg 350.0 200 ] |> List.map (\peg -> { peg | pegType = MultiBall })
                ]
        , walls = sideWalls
        }


centerPegs : List Peg -> List Peg
centerPegs pegs =
    let

        positions =
            pegs |> List.map .position |> List.map getX

        offset = case (List.maximum positions, List.minimum positions) of
          (Just max, Just min) -> (Bounds.gameX - (max - min)) / 2
          _ -> 0

    in
        pegs |> List.map (addPegX offset)


pyramidLevel : LevelDef
pyramidLevel =
    let
        spacing =
              50

        rowFunc row =
              hLineOffPegs row spacing
                  |> addPegYs ((toFloat row) * spacing + 50)


        pegs_ =
            List.range 1 7
                |> List.map (rowFunc >> centerPegs)
                |> List.concat
                |> randomRedPegs
    in
        { pegs = pegs_
        , walls = sideWalls
        }

veeLevel : LevelDef
veeLevel =
    let
        spacing =
            50

        rowFunc row =
            hLineOffPegs (8 - row) spacing
                |> addPegYs ((toFloat row) * spacing + 100)

        pegs_ =
            List.range 1 7
                |> List.map (rowFunc >> centerPegs)
                |> List.concat
                |> randomRedPegs

    in
        { pegs = pegs_
        , walls = sideWalls
        }


allLevels : List LevelDef
allLevels =
    [ veeLevel, pyramidLevel, circleMultiBallLevel, bewbsLevel ]
