module World.Casting exposing (..)

import World exposing (..)
import Slime exposing (..)
import World.Components exposing (..)
import World.Input as Input
import Vector2 exposing (..)
import Math exposing (center)


spawn : Float2 -> Float -> EntityID -> Float2 -> PlayerProjectile
spawn pos speed owner target =
    { pos = pos
    , vel = scale speed (directionFromTo pos target)
    , owner = owner
    , lifeLeft = 1
    , damage = 2
    }


playerCasting : Float -> World -> World
playerCasting delta world =
    let
        players =
            world &. (entities2 player transforms)

        target =
            Input.gameMouse world

        shoot { a, b, id } world =
            let
                playerCenter =
                    center b

                ( _, updatedWorld ) =
                    forNewEntity world
                        &=> ( playerProjectiles, spawn playerCenter 8 id target )
            in
                updatedWorld

        maybeShoot ({ a, id } as ent) world =
            let
                newTime =
                    a.currentTime + delta

                wantToShoot =
                    world.inputState.mouseDown
                        && (a.lastCast + a.castSpeed < newTime)

                updatedWorld =
                    if wantToShoot then
                        shoot ent world
                    else
                        world

                updateTime player =
                    { player
                        | currentTime = newTime
                        , lastCast =
                            if wantToShoot then
                                player.lastCast + player.castSpeed
                            else if world.inputState.mouseDown then
                                player.lastCast
                            else
                                newTime
                    }

                ( _, updatedWorld2 ) =
                    forEntityById id updatedWorld
                        &~> ( player
                            , Maybe.map updateTime
                            )
            in
                updatedWorld2

        updatedWorld =
            List.foldl maybeShoot world players
    in
        updatedWorld
