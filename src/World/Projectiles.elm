module World.Projectiles exposing (..)

import World exposing (..)
import World.Components exposing (Projectile, OwnedProjectile)
import Slime exposing (..)
import Vector2 exposing (..)
import Math exposing (Rectangle, segmentCircle)


collides : Float -> Projectile x -> Rectangle -> Bool
collides delta { pos, vel } target =
    segmentCircle pos (scale delta vel) target


playerProjectileStep : Float -> World -> ( World, List EntityID )
playerProjectileStep delta world =
    let
        targets =
            world &. (entities2 enemies transforms)

        hitTarget ({ b } as target) ( { a, id } as me, hitSoFar ) =
            let
                projectile =
                    a

                targetBox =
                    b
            in
                if collides delta projectile b then
                    ( me, [ target.id, id ] ++ hitSoFar )
                else
                    ( me, hitSoFar )

        hitTargets ( me, hitSoFar ) =
            List.foldr hitTarget ( me, hitSoFar ) targets

        ( updatedWorld, hitTargets ) =
            stepEntitiesWith (entities playerProjectiles) [] world
    in
        ( updatedWorld, hitTargets )
