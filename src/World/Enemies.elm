module World.Enemies exposing (..)

import Slime exposing (..)
import World exposing (..)
import World.Components exposing (Enemy(..))
import World.View exposing (getScreenCenter)
import World.Spawning exposing (Spawner, radialSpawner, spawningSystem)
import Vector2 exposing (..)


spawnGoblin : ( ( Float, Float ), Float ) -> World -> World
spawnGoblin ( ( x, y ), roll ) world =
    let
        ( _, updatedWorld ) =
            forNewEntity world
                &=> ( enemies, Goblin )
                &=> ( transforms, { x = x, y = y, width = 1, height = 1 } )
    in
        updatedWorld


chasePlayer : Float -> World -> World
chasePlayer delta world =
    let
        playerLocations =
            (world &. (entities2 player transforms))
                |> List.map .b

        chase ({ b } as ent) =
            let
                transform =
                    b

                moveTowards rate target me =
                    let
                        ( dx, dy ) =
                            scale rate (directionFromTo ( me.x, me.y ) ( target.x, target.y ))
                    in
                        { me | x = me.x + dx, y = me.y + dy }

                moved =
                    List.foldl (moveTowards (delta * 4)) transform playerLocations
            in
                { ent | b = moved }
    in
        stepEntities (entities2 enemies transforms) chase world


spawns : List (Spawner World)
spawns =
    let
        centerer =
            getScreenCenter

        distancer =
            (\_ -> 12)

        spawner =
            spawnGoblin

        chancer =
            (\_ -> 0)
    in
        [ radialSpawner
            { centerer = centerer
            , distancer = distancer
            , spawner = spawner
            , chancer = chancer
            }
        ]
