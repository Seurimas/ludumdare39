module World.Movement exposing (..)

import Slime exposing (..)
import World exposing (..)
import World.Components exposing (..)
import Vector2 exposing (..)
import Math exposing (..)
import World.Input exposing (InputState, gameMouse)
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
        if x /= 0 || y /= 0 then
            normalize ( x, y )
        else
            ( 0, 0 )


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
                    if me.moveSpeed > me.maxMoveSpeed then
                        me.moveSpeed
                    else
                        min (me.moveSpeed + delta) me.maxMoveSpeed

                change =
                    scale (delta * playerSpeed) disposition
            in
                { ent | b = moveRectangle change b, a = { a | moveSpeed = playerSpeed } }
    in
        stepEntities (entities2 player transforms) moveMe world


facePlayer : World -> World
facePlayer world =
    let
        target =
            gameMouse world

        facing ({ b } as ent) =
            let
                me =
                    center b

                w =
                    sub target me

                rotation =
                    angleOf w
            in
                { ent | c = rotation }
    in
        stepEntities (entities3 player transforms rotations) facing world


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
