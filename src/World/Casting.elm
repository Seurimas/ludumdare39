module World.Casting exposing (..)

import World exposing (..)
import Slime exposing (..)
import World.Components exposing (..)
import World.Input as Input
import Vector2 exposing (..)


playerCasting : Float -> World -> World
playerCasting delta world =
    let
        players =
            world &. (entities2 player transforms)

        target =
            Input.gameMouse world

        spawn pos speed owner =
            { pos = pos
            , vel = scale speed (directionFromTo pos target)
            , owner = owner
            }

        shoot { a, b, id } world =
            if wantToShoot a then
                let
                    playerCenter =
                        center b

                    ( _, updatedWorld ) =
                        forNewEntity
                            &=> ( playerProjectiles, spawn playerCenter id )
                in
                    updatedWorld
            else
                world

        updatedWorld =
            List.foldl shoot world players
    in
        updatedWorld
