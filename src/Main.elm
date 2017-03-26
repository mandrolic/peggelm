import Model exposing (..)
import Html
import View
import Task
import Types exposing (..)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Window

subscriptions : a -> Sub Msg
subscriptions _ =
  -- Time.every (16.667 * Time.millisecond) (\t -> Model.Tick  16.667)
  Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes sizeToMsg
        , Keyboard.downs KeyDown
        ]


-- WINDOW RESIZE

initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg Window.size


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    WindowResize size

main : Program Never Model Msg
main =
  Html.program
    { init = Model.initial
       ! [
             initialSizeCmd
           ]
    , view = View.view
    , update = Model.update
    , subscriptions = subscriptions
    }



{-
TODO:



* Aimving should be a timing process and the barrel goes side to sideWalls
* Sinusoidal funtion for movement of bucket
* 'Far shot' bonuses etc

Bugz:

* Ball travelling fast can zoom through a peg


-}
