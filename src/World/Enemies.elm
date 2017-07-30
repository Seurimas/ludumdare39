module World.Enemies exposing (..)

import Slime exposing (..)
import World exposing (..)
import World.Components exposing (Enemy)
import World.View exposing (getScreenCenter)
import World.Spawning exposing (Spawner, radialSpawner, spawningSystem)
import Vector2 exposing (..)
import Math exposing (separation, moveRectangle)
import Assets.Reference exposing (..)


initGoblin : Enemy
initGoblin =
    { moveSpeed = 5
    , attackDamage = 1
    , health = 4
    , maxHealth = 4
    , sprite = Goblin
    , attackProgress = Nothing
    , attackSpeed = 0.75
    }


initHobgoblin : Enemy
initHobgoblin =
    { moveSpeed = 3
    , attackDamage = 2
    , health = 6
    , maxHealth = 6
    , sprite = Hobgoblin
    , attackProgress = Nothing
    , attackSpeed = 1.25
    }


initHulk : Enemy
initHulk =
    { moveSpeed = 3
    , attackDamage = 4
    , health = 14
    , maxHealth = 14
    , sprite = Hulk
    , attackProgress = Nothing
    , attackSpeed = 1.25
    }


getFrame { attackProgress, attackSpeed } =
    case attackProgress of
        Just progress ->
            (floor ((progress / attackSpeed) * 2 + 1)) % 3

        Nothing ->
            0


spawnEnemy : Enemy -> ( Float, Float ) -> ( ( Float, Float ), Float ) -> World -> World
spawnEnemy init ( width, height ) ( ( x, y ), roll ) world =
    let
        ( _, updatedWorld ) =
            forNewEntity world
                &=> ( enemies, init )
                &=> ( transforms, { x = x, y = y, width = width, height = height } )
                &=> ( rotations, 0 )
    in
        updatedWorld


spawnGoblin =
    spawnEnemy initGoblin ( 0.9, 0.9 )


spawnHobgoblin =
    spawnEnemy initHobgoblin ( 1.1, 1.1 )


spawnHulk =
    spawnEnemy initHulk ( 1.2, 1.2 )


cullDead : World -> ( World, List EntityID )
cullDead world =
    let
        deletes =
            (world &. (entities enemies))
                |> List.filter (\{ a } -> a.health <= 0)
                |> List.map .id
    in
        ( world, deletes )


damageSideEffect : Float -> EntityID -> World -> World
damageSideEffect amount id world =
    let
        ( _, updatedWorld ) =
            forEntityById id world
                &~> ( player, Maybe.map (World.Components.hurt amount) )
    in
        updatedWorld


chasePlayer : Float -> World -> World
chasePlayer delta world =
    let
        players =
            (world &. (entities2 player transforms))

        playerLocations =
            players |> List.map .b

        chase ({ a, b, c } as ent) =
            let
                transform =
                    b

                moveTowards rate target ( me, rotation ) =
                    let
                        ( dx, dy ) =
                            directionFromTo ( me.x, me.y ) ( target.x, target.y )
                    in
                        ( { me | x = me.x + rate * dx, y = me.y + rate * dy }
                        , Math.angleOf ( dx, dy )
                        )

                ( moved, rotated ) =
                    List.foldl (moveTowards (delta * a.moveSpeed)) ( transform, c ) playerLocations
            in
                { ent | b = moved, c = rotated }

        attack ({ a } as me) target =
            ( { me | a = { a | attackProgress = Just 0 } }
            , damageSideEffect a.attackDamage target.id
            )

        fight player ( { a, b, c } as me, sideEffects ) =
            let
                { attackProgress, attackSpeed } =
                    a
            in
                case attackProgress of
                    Just progress ->
                        if progress > attackSpeed then
                            ( { me | a = { a | attackProgress = Nothing } }
                            , sideEffects
                            )
                        else
                            ( { me | a = { a | attackProgress = Just (progress + delta) } }
                            , sideEffects
                            )

                    Nothing ->
                        if distance (Math.center b) (Math.center player.b) < (player.b.width / 2 + b.width / 2) + 0.1 then
                            attack me player
                        else
                            ( me, sideEffects )

        chaseOrFight ( { a, b, c } as ent, sideEffects ) =
            let
                ( fightingIfAble, damage ) =
                    List.foldl fight ( ent, sideEffects ) players

                chasingOtherwise =
                    case a.attackProgress of
                        Just _ ->
                            fightingIfAble

                        Nothing ->
                            chase fightingIfAble
            in
                ( chasingOtherwise, damage )

        ( steppedWorld, sideEffects ) =
            stepEntitiesWith (entities3 enemies transforms rotations) chaseOrFight ( world, identity )
    in
        sideEffects steppedWorld


separateEnemies : Float -> World -> World
separateEnemies delta world =
    let
        collidables =
            ((world &. (entities2 enemies transforms))
                |> List.map .b
            )
                ++ ((world &. (entities2 player transforms))
                        |> List.map .b
                   )

        push ({ b } as me) =
            let
                transform =
                    b

                pushes =
                    List.map (separation transform) collidables

                pushVector =
                    List.foldl
                        (\separation force ->
                            case separation of
                                Just vec ->
                                    (add vec force)

                                Nothing ->
                                    force
                        )
                        ( 0, 0 )
                        pushes
                        |> scale 0.5
            in
                { me
                    | b = moveRectangle pushVector b
                }
    in
        stepEntities (entities2 enemies transforms) push world


spawns : List (Spawner World)
spawns =
    let
        baseSpawnChance =
            0.75
    in
        [ radialSpawner
            { centerer = getScreenCenter
            , distancer = (\_ -> 12)
            , spawner = spawnGoblin
            , chancer = (\_ -> baseSpawnChance)
            }
        , radialSpawner
            { centerer = getScreenCenter
            , distancer = (\_ -> 11)
            , spawner = spawnHobgoblin
            , chancer = (\_ -> baseSpawnChance + 0.33 * (1 - baseSpawnChance))
            }
        , radialSpawner
            { centerer = getScreenCenter
            , distancer = (\_ -> 10)
            , spawner = spawnHulk
            , chancer = (\_ -> baseSpawnChance + 0.66 * (1 - baseSpawnChance))
            }
        ]
