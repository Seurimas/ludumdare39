module World.Projectiles exposing (..)

import World exposing (..)
import World.Components exposing (Projectile, PlayerProjectile, Enemy, OwnedProjectile, SpellType)
import Slime exposing (..)
import Vector2 exposing (..)
import Math exposing (Rectangle, segmentCircle)
import World.Spells exposing (spellToEffect)


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


playerProjectileStep : Float -> World -> ( World, List EntityID )
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
                        ( newSideEffects, removeProjectile ) =
                            spellToEffect me me.a.effect { id = target.id, damagable = target.a }

                        withProjectile =
                            if removeProjectile then
                                id :: hitSoFar
                            else
                                hitSoFar
                    in
                        ( me, ( sideEffects >> newSideEffects, withProjectile ) )
                else
                    ( me, ( sideEffects, hitSoFar ) )

        hitTargets ( me, ( sideEffects, hitSoFar ) ) =
            List.foldr hitTarget ( me, ( sideEffects, hitSoFar ) ) targets

        ( updatedWorld, ( sideEffects, hit ) ) =
            stepEntitiesWith (entities playerProjectiles) hitTargets ( world, ( identity, [] ) )
    in
        ( sideEffects updatedWorld, hit )
