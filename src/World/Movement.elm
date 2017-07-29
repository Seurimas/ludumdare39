module World.Movement exposing (..)

import Slime exposing (..)
import World exposing (..)
import Collision2D exposing (rectangleSide, Side)
import Vector2 exposing (..)


getDisposition : InputState -> Float2
getDisposition { left, right, up, down } =
    let
        x =
            if left then
                -1
            else if right then
                1
            else
                0

        y =
            if up then
                1
            else if down then
                -1
            else
                0
    in
        ( x, y )


movePlayer : Float -> World -> World
movePlayer delta world =
    let
        moveMe ({ a, b } as ent) =
            let
                me =
                    a

                transform =
                    b

                disposition =
                    getDisposition world.inputState

                playerSpeed =
                    getPlayerSpeed me

                change =
                    scale playerSpeed disposition
            in
                { ent | b = moveRectangle change b }
    in
        stepEntities (entities player transforms) moveMe
