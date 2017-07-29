module World.Projectiles exposing (..)

import World exposing (..)
import World.Components exposing (Projectile, PlayerProjectile, Enemy, OwnedProjectile, hurt)
import Slime exposing (..)
import Vector2 exposing (..)
import Math exposing (Rectangle, segmentCircle)


collides : Float -> Projectile x -> Rectangle -> Bool
collides delta { pos, vel } target =
    segmentCircle pos (scale delta vel) target


projectileStep : ComponentSpec (Projectile x) World -> Float -> World -> ( World, List EntityID )
projectileStep projectileType delta world =
    let
        stepProjectile ( { a, id } as ent, deletes ) =
            let
                { pos, vel, lifeLeft } =
                    a

                newA =
                    { a
                        | pos = add pos (scale delta vel)
                        , lifeLeft = lifeLeft - delta
                    }
            in
                ( { ent | a = newA }
                , if newA.lifeLeft <= 0 then
                    id :: deletes
                  else
                    deletes
                )
    in
        stepEntitiesWith (entities projectileType) stepProjectile ( world, [] )


doProjectile : Entity PlayerProjectile -> Entity2 Enemy x -> World -> World
doProjectile proj target world =
    let
        ( _, updatedWorld ) =
            forEntityById target.id world
                &~> ( enemies, Maybe.map (hurt proj.a.damage) )
    in
        updatedWorld


despawnProjectile : Entity (OwnedProjectile x) -> Entity2 y z -> Bool
despawnProjectile proj target =
    True


despawnTarget : Entity (OwnedProjectile x) -> Entity2 y z -> Bool
despawnTarget proj target =
    False


hitEnemy : Entity PlayerProjectile -> Entity2 Enemy x -> ( World -> World, Bool, Bool )
hitEnemy proj ({ id } as target) =
    let
        hit =
            doProjectile proj target
    in
        ( hit, despawnProjectile proj target, despawnTarget proj target )


playerProjectileStep : Float -> World -> ( World, World -> World, List EntityID )
playerProjectileStep delta world =
    let
        targets =
            world &. (entities2 enemies transforms)

        hitTarget ({ b } as target) ( { a, id } as me, ( sideEffects, hitSoFar ) ) =
            let
                projectile =
                    a

                targetBox =
                    b
            in
                if collides delta projectile b then
                    let
                        ( newSideEffects, removeProjectile, removeTarget ) =
                            hitEnemy me target

                        withProjectile =
                            if removeProjectile then
                                id :: hitSoFar
                            else
                                hitSoFar

                        withTarget =
                            if removeTarget then
                                target.id :: withProjectile
                            else
                                withProjectile
                    in
                        ( me, ( sideEffects >> newSideEffects, withTarget ) )
                else
                    ( me, ( sideEffects, hitSoFar ) )

        hitTargets ( me, ( sideEffects, hitSoFar ) ) =
            List.foldr hitTarget ( me, ( sideEffects, hitSoFar ) ) targets

        ( updatedWorld, ( sideEffects, hit ) ) =
            stepEntitiesWith (entities playerProjectiles) hitTargets ( world, ( identity, [] ) )
    in
        ( updatedWorld, sideEffects, hit )
