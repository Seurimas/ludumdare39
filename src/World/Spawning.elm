module World.Spawning exposing (..)

import Random.Pcg as Pcg exposing (Seed, initialSeed)


type alias SpawningWorld x =
    { x
        | seed : Seed
        , lastSpawn : Float
        , currentTime : Float
        , interval : Float
    }


initSeed =
    Pcg.initialSeed 8675309


type alias RadialSpawnSpec world =
    { centerer : world -> ( Float, Float )
    , distancer : world -> Float
    , spawner : ( ( Float, Float ), Float ) -> (world -> world)
    , chancer : world -> Float
    }


type alias SparseSpawnerSpec world =
    { placer : world -> Maybe ( ( Float, Float ), world )
    , checker : world -> Bool
    , spawner : ( ( Float, Float ), Float ) -> (world -> world)
    , chancer : world -> Float
    }


type alias Spawner world =
    Float -> world -> world


radialSpawner :
    RadialSpawnSpec (SpawningWorld world)
    -> Float
    -> SpawningWorld world
    -> SpawningWorld world
radialSpawner { centerer, distancer, spawner, chancer } roll ({ seed } as world) =
    let
        ( centerX, centerY ) =
            centerer world

        distance =
            distancer world

        chance =
            chancer world
    in
        if chance > roll then
            world
        else
            let
                ( location, steppedSeed ) =
                    Pcg.step
                        (Pcg.float -pi pi
                            |> Pcg.map (\radial -> ( (sin radial * distance) + centerX, (cos radial * distance) + centerY ))
                        )
                        seed

                ( spawnRoll, updatedSeed ) =
                    Pcg.step
                        (Pcg.float 0 1)
                        seed

                spawned =
                    spawner ( location, spawnRoll ) world
            in
                { spawned | seed = updatedSeed }


sparseSpawner :
    SparseSpawnerSpec (SpawningWorld world)
    -> Float
    -> SpawningWorld world
    -> SpawningWorld world
sparseSpawner { placer, checker, spawner, chancer } roll world =
    let
        chance =
            chancer world

        wantSpawn =
            chance < roll && checker world
    in
        if not wantSpawn then
            world
        else
            case placer world of
                Just ( location, steppedWorld ) ->
                    let
                        ( spawnRoll, updatedSeed ) =
                            Pcg.step
                                (Pcg.float 0 1)
                                steppedWorld.seed

                        spawned =
                            spawner ( location, spawnRoll ) steppedWorld
                    in
                        { spawned | seed = updatedSeed }

                _ ->
                    world


rollSpawner : Spawner (SpawningWorld world) -> SpawningWorld world -> SpawningWorld world
rollSpawner spawner ({ seed } as world) =
    let
        ( roll, updatedSeed ) =
            Pcg.step (Pcg.float 0 1) seed
    in
        spawner roll { world | seed = updatedSeed }


spawningSystem : List (Spawner (SpawningWorld world)) -> Float -> SpawningWorld world -> SpawningWorld world
spawningSystem spawners delta ({ currentTime, lastSpawn, interval } as world) =
    let
        newTime =
            currentTime + delta

        shouldSpawnWave =
            lastSpawn + interval < currentTime

        spawnedWorld =
            if shouldSpawnWave then
                List.foldl rollSpawner world spawners
            else
                world
    in
        { spawnedWorld
            | currentTime = newTime
            , lastSpawn =
                if shouldSpawnWave then
                    currentTime + interval
                else
                    lastSpawn
        }
