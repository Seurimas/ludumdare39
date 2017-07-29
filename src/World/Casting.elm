module World.Casting exposing (..)

import World exposing (..)
import Slime exposing (..)
import World.Components exposing (..)
import World.Input as Input

playerCasting : Float -> World -> World
playerCasting delta world =
    let
        target e = Input.gameMouse world
        shoot ({b}) world =
            let
                playerCenter = center b
