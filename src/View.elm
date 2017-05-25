module View exposing (view)

import Basics exposing (toFloat)
import Bounds
import Html exposing (Html, a, div, img, span, text)
import Json.Decode as Json
import Math.Vector2 exposing (..)
import String exposing (join)
import Svg exposing (Svg, circle, rect, svg, tspan)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Types exposing (..)
import VectorHelpers exposing (..)
import SvgHelpers exposing (..)


--import Mouse exposing (Position)


view : Model -> Html Msg
view model =
    div
        [--class "main"
        ]
        [ viewPlayArea model
        ]


viewPlayArea : Model -> Html Msg
viewPlayArea model =
    let
        ( gameDimAttrib, ratio ) =
            gameDimensions model.windowWidth model.windowHeight

        svgBackground =
            rect
                [ class "game-background"
                , width "100%"
                , height "100%"
                  --  , Svg.Events.onMouseUp Model.UserClicked
                , Svg.Events.on "mousemove" (decodeUserMouseMoveMsg ratio)
                , Svg.Events.on "mouseup" (decodeUserClickedMsg ratio)
                ]
                []
    in
        Svg.svg
            [ class "game-svg"
            , viewBox ("0 0 " ++ (toString Bounds.gameX) ++ " " ++ (toString Bounds.gameY))
            , gameDimAttrib
            ]
        <|
            List.concat
                [ [ svgBackground ]
                , List.map (viewWall ratio) model.walls
                , List.map (viewPeg ratio) model.pegs
                , List.map (viewBall ratio) model.balls
                , [ viewBarrel ratio model.barrelAngle ]
                , [ viewBucket model.bucket ]
                , List.map viewScoreMarker model.scoreMarkers
                , concatIf (model.gameState == GameOver) (svgGameOver ratio)
                    [ svgScore ratio model.score model.pegs model.redPegTargetForCurrentLevel
                      , svgBallsLeft model.ballsLeft
                    ]
                ]


concatIf : Bool -> a -> List a -> List a
concatIf predicate item list =
    if predicate then
        List.append [ item ] list
    else
        list


viewPeg : Float -> Peg -> Svg Msg
viewPeg ratio peg =
    let
        pegTypeClass =
            case peg.pegType of
                Red ->
                    "redPeg"

                MultiBall ->
                    "multiBallPeg"

                _ ->
                    ""

        staticAtts =
            [ cx (peg.position |> getXStr)
            , cy (peg.position |> getYStr)
            ]

        conditionalClasses =
            [ ( peg.hitCount > 0, "hit" ) ]

        classAtt =
            class <| "peg " ++ pegTypeClass ++ " " ++ (join " " (conditionalClasses |> List.filter Tuple.first |> List.map Tuple.second))
    in
        Svg.g []
            [ -- peg
              Svg.circle ((r (toString peg.radius)) :: classAtt :: staticAtts) []
              -- score marker (when present)
            , Svg.text_
                [ class "score-text"
                , opacity
                    (toString
                        (if peg.scoreDisplayTimeLeft > 0 then
                            1
                         else
                            0
                        )
                    )
                , textAnchor "middle"
                , dominantBaseline "middle"
                , y (getYStr (addY 16 peg.position))
                , x (getXStr peg.position)
                ]
                [ Svg.text (toString peg.scoreLastHit) ]
            ]


viewScoreMarker : ScoreMarker -> Svg Msg
viewScoreMarker scoreMarker =
    let
        classAtt =
            class "scoremarker"

        positionAtts =
            [ cx (scoreMarker.position |> getXStr)
            , cy (scoreMarker.position |> getYStr)
            ]
    in
        Svg.g
            [ transform (tfm [ (translateVec2 scoreMarker.position) ])
            , opacity <| toString <| scoreMarker.opacity
            ]
            [ --Svg.circle ([ ( r (toString scoreMarker.radius)), classAtt ] ) []
              Svg.text_
                [ class "bonus-popup", textAnchor "middle", dominantBaseline "middle" ]
                [ Svg.text scoreMarker.text ]
            ]


viewBucket : Bucket -> Svg Msg
viewBucket bucket =
    Svg.rect
        [ class "bucket"
        , x <| toString <| bucket.xOffset
        , y <| toString <| Bounds.gameY - 5
        , width <| toString <| bucket.width
        , height "5"
        ]
        []


viewBarrel : Float -> Float -> Svg Msg
viewBarrel ratio barrelAngle =
    let
        barrelWidth =
            12

        gunPosition =
            Bounds.gameX / 2
    in
        Svg.g []
            [ Svg.rect
                [ class "gunbarrel"
                , transform (tfm [ Rotate barrelAngle gunPosition 0.0 ])
                , x (toString gunPosition)
                , y (toString (0.0 - (barrelWidth / 2)))
                , width "40"
                , height (toString barrelWidth)
                ]
                []
            , Svg.circle
                [ class "gunbase"
                , cx (toString gunPosition)
                , cy "0"
                , r "30"
                ]
                []
            ]


viewBall : Float -> Ball -> Svg Msg
viewBall ratio ball =
    Svg.circle
        [ class "ball"
        , cx (ball.physics.position |> getXStr)
        , cy (ball.physics.position |> getYStr)
        , r (toString ball.physics.radius)
        ]
        []


viewWall : Float -> VerticalWall -> Svg Msg
viewWall ratio wall =
    Svg.rect
        [ class "wall"
        , x (wall.hPos |> toString)
        , y (wall.upperBound |> toString)
        , width (toString wall.width)
        , height (toString (wall.lowerBound - wall.upperBound))
        ]
        []


svgGameTextLine : String -> Svg a
svgGameTextLine text =
    let
        screenCentre =
            toString (Bounds.gameX / 2)
    in
        Svg.tspan
            [ dy "60"
            , x screenCentre
            , textAnchor "middle"
            ]
            [ Svg.text text ]


svgScore : Float -> Int -> List Peg -> Int -> Svg a
svgScore ratio score pegs requiredRedPegs =
  let
    redPegsRemaining = pegs |> List.filter (\p -> p.pegType == Red) |> List.length
    redPegsHit = requiredRedPegs - redPegsRemaining

    mainScore =   Svg.text_
          [ y "0"
          , class "game-text"
          ]
          [ Svg.tspan
              [ dy "30"
              , x "10"
              , textAnchor "start"
              ]
              [ Svg.text <| "Score: " ++ (toString score) ]
         ]

    pegsNeeded = Svg.text_
          [ y "0"
          , class "game-text"
          ]
          [
          Svg.tspan
            [ dy "10"
            , x "25"
            , textAnchor "start"
            , fontSize "16"
            ]
            [ Svg.text <| (toString redPegsHit) ++  " of " ++ (toString requiredRedPegs) ]
          ]

    examplePeg = Svg.circle [ r "6",  class "peg redPeg",  cx "15", cy "5"] []
  in
    Svg.g []
    [
      mainScore
      , Svg.g [  transform (tfm [ Translate 0 40 ]) ]
        [
        examplePeg
        ,pegsNeeded
        ]
    ]

svgBallsLeft : Int -> Svg a
svgBallsLeft ballsLeft =
    Svg.text_
        [ y "0"
        , class "game-text"
        ]
        [ Svg.tspan
            [ dy "30"
              -- <-- AR: what this do?
            , x <| toString (Bounds.gameX - 10)
            , textAnchor "end"
            ]
            [ Svg.text <| "Balls: " ++ (toString ballsLeft) ]
        ]


svgGameOver : Float -> Svg Msg
svgGameOver backgroundTextOpacity =
    let
        rectWidth =
            200

        rectLeft =
            (Bounds.gameX - rectWidth) / 2
    in
        Svg.g
            [--transform  (tfm [ (Translate rectLeft 0) ])
            ]
            [ rect
                [ Svg.Attributes.fill "black"
                , fillOpacity "0.9"
                , width (toString Bounds.gameX)
                , height (toString Bounds.gameY)
                , Svg.Events.onClick PlayAgain
                ]
                []
            , Svg.text_
                [ class "game-text"
                , y (toString ((Bounds.gameY / 2) - 60))
                , strokeOpacity (toString backgroundTextOpacity)
                ]
                (List.map svgGameTextLine [ "GAME", "OVER" ])
            ]


gameDimensions : Int -> Int -> ( Svg.Attribute msg, Float )
gameDimensions windowWidth windowHeight =
    let
        gameRatio =
            Bounds.gameX / Bounds.gameY

        windowMargin =
            48

        width =
            windowWidth - windowMargin

        height =
            windowHeight - windowMargin

        windowRatio =
            toFloat width / toFloat height
    in
        if windowRatio > gameRatio then
            ( Svg.Attributes.height ((toString height) ++ "px"), toFloat height / Bounds.gameY )
        else
            ( Svg.Attributes.width ((toString width) ++ "px"), toFloat width / Bounds.gameX )


getXStr : Vec2 -> String
getXStr =
    getX >> toString


getYStr : Vec2 -> String
getYStr =
    getY >> toString


getClickPos : Json.Decoder ( Int, Int )
getClickPos =
    Json.map2 (,)
        (Json.at [ "offsetX" ] Json.int)
        (Json.at [ "offsetY" ] Json.int)


getClickPosWithButtons : Json.Decoder ( Int, Int, Int )
getClickPosWithButtons =
    Json.map3 (,,)
        (Json.at [ "offsetX" ] Json.int)
        (Json.at [ "offsetY" ] Json.int)
        (Json.at [ "buttons" ] Json.int)


getCoords : Float -> ( Int, Int, Int ) -> Vec2
getCoords ratio ( x, y, buttons ) =
    Math.Vector2.fromTuple ( (toFloat x / ratio), (toFloat y / ratio) )


decodeUserMouseMoveMsg : Float -> Json.Decoder Msg
decodeUserMouseMoveMsg ratio =
    getClickPosWithButtons
        |> Json.map (getCoords ratio)
        |> Json.map MouseMoved


decodeUserClickedMsg : Float -> Json.Decoder Msg
decodeUserClickedMsg ratio =
    getClickPosWithButtons
        |> Json.map (getCoords ratio)
        |> Json.map UserClicked
