module World.Casting exposing (..)

import World exposing (..)
import Slime exposing (..)
import World.Components exposing (..)
import World.Input as Input
import Vector2 exposing (..)
import Math exposing (center)
import Assets.Reference exposing (playSfx)
import Whistle.Types exposing (RawNode)


spawn : ProjSpell -> Float2 -> EntityID -> Float2 -> PlayerProjectile
spawn spell pos owner target =
    { pos = pos
    , vel = scale spell.projSpeed (directionFromTo pos target)
    , owner = owner
    , lifeLeft = 1
    , effect = Proj spell
    , sprite = spell.projectileArt
    }


shoot target projSpell caster world =
    let
        playerCenter =
            center caster.b

        ( _, updatedWorld ) =
            forNewEntity world
                &=> ( playerProjectiles, spawn projSpell playerCenter caster.id target )
    in
        updatedWorld


place target areaSpell caster world =
    let
        ( _, updatedWorld ) =
            forNewEntity world
    in
        updatedWorld


playerCasting : (RawNode -> msg) -> Float -> World -> ( World, Cmd msg )
playerCasting playMsg delta world =
    let
        players =
            world &. (entities2 player transforms)

        target =
            Input.gameMouse world

        cast spell caster world =
            case spell of
                Proj projSpell ->
                    shoot target projSpell caster world

                Area areaSpell ->
                    place target areaSpell caster world

        maybeShoot ({ a, id } as ent) ( world, played ) =
            let
                newTime =
                    a.currentTime + delta

                wantToShoot =
                    world.inputState.mouseDown
                        && (a.lastCast + a.spell.castSpeed < newTime)

                updatedWorld =
                    if wantToShoot then
                        cast ent.a.spell.effect ent world
                    else
                        world

                deplete spell =
                    { spell | castsLeft = spell.castsLeft - 1 }

                updateTime player =
                    { player
                        | currentTime = newTime
                        , lastCast =
                            if wantToShoot then
                                player.lastCast + player.spell.castSpeed
                            else if world.inputState.mouseDown then
                                player.lastCast
                            else
                                newTime
                        , spell =
                            if wantToShoot then
                                deplete a.spell
                            else
                                a.spell
                    }

                ( _, updatedWorld2 ) =
                    forEntityById id updatedWorld
                        &~> ( player
                            , Maybe.map updateTime
                            )
            in
                ( updatedWorld2
                , if wantToShoot then
                    playSfx playMsg a.spell.sound 0.125 world.assets :: played
                  else
                    played
                )

        ( updatedWorld, sounds ) =
            List.foldl maybeShoot ( world, [] ) players
    in
        ( updatedWorld, Cmd.batch sounds )
