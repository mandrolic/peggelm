{-

Functions not found in elm Basic libs

-}
module BasicHelpers exposing (..)

import Task
import Process
import Time exposing (..)

consMaybe : Maybe a -> List a -> List a
consMaybe = Maybe.map (::) >> Maybe.withDefault identity
--consMaybe m xs =  Maybe.withDefault (Maybe.map (\x -> x :: xs) m)  xs



doAfterDelay : a -> Float -> Cmd a
doAfterDelay msg delayMs =
    Task.perform (\_ -> msg) (Process.sleep (delayMs * millisecond))
