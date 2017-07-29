module World.Movement exposing (..)

import Slime exposing (..)
import World exposing (..)
import World.Components exposing (..)
import Vector2 exposing (..)
import Math exposing (..)
import World.Input exposing (InputState)
import Game.TwoD.Camera exposing (follow)


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
                    me.moveSpeed

                change =
                    scale (delta * playerSpeed) disposition
            in
                { ent | b = moveRectangle change b }
    in
        stepEntities (entities2 player transforms) moveMe world


cameraFollow : Float -> World -> World
cameraFollow delta world =
    let
        followPlayer ( { a, b } as ent, camera ) =
            ( ent, follow 4 delta ( b.x, b.y ) camera )

        ( _, updatedCamera ) =
            stepEntitiesWith (entities2 player transforms) followPlayer ( world, world.camera )
    in
        { world
            | camera = updatedCamera
        }
