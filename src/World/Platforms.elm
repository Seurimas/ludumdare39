module World.Platforms exposing (..)

import Slime exposing (..)
import World.Spawning exposing (..)
import World.Components exposing (..)
import World.Spells exposing (..)
import World exposing (..)
import Math exposing (..)
import Assets.Reference as Reference
import Random.Pcg as Pcg
import Vector2 exposing (..)


removeSpell spell spells =
    List.filter (\( cost, checked ) -> checked /= spell) spells


requiredProgress spell spells =
    List.filter (\( cost, checked ) -> checked == spell) spells
        |> List.head
        |> Maybe.map (\( required, spell ) -> required)
        |> Maybe.withDefault 0


pickSpell : Spell -> World -> World
pickSpell newSpell world =
    let
        getSpell ( { a } as platform, found ) =
            if a.progress > requiredProgress newSpell a.spells then
                ( { platform | a = { a | spells = removeSpell newSpell a.spells } }, True )
            else
                ( platform, found )

        ( updatedPlatforms, accepted ) =
            stepEntitiesWith (entities platforms) getSpell ( world, False )

        updatedPlayers =
            if accepted then
                stepEntities (entities player) (\({ a } as ent) -> { ent | a = { a | spell = newSpell } }) updatedPlatforms
            else
                updatedPlatforms
    in
        updatedPlayers


initPlatform : List ( Float, Spell ) -> Platform
initPlatform spells =
    { sprite = Reference.Platform
    , progress = 0
    , spells = spells
    }


spawnPlatform : ( Float2, Float ) -> World -> World
spawnPlatform ( place, factor ) world =
    let
        ( spells, updatedSeed ) =
            spellDrops world

        ( _, updatedWorld ) =
            forNewEntity world
                &=> ( transforms, { x = getX place - 2, y = getY place - 2, width = 4, height = 4 } )
                &=> ( platforms, initPlatform spells )
    in
        { updatedWorld | magicSeed = updatedSeed }


getSpellPicks : World -> ( Float, List ( Float, Spell ) )
getSpellPicks world =
    let
        onPlatforms =
            platformsNear ( 1, 1 ) world
    in
        onPlatforms
            |> List.map (\{ a } -> ( a.progress, a.spells ))
            |> List.head
            |> Maybe.withDefault ( 0, [] )


progressPlatforms : Float -> World -> World
progressPlatforms delta world =
    let
        platformsByPlayer =
            platformsNear ( 1, 1 ) world

        updatePlatform platform lastWorld =
            let
                ( _, nextWorld ) =
                    forEntityById platform.id lastWorld
                        &~> ( platforms, Maybe.map (\platform -> { platform | progress = platform.progress + delta }) )
            in
                nextWorld

        updatedWorld =
            List.foldl updatePlatform world platformsByPlayer
    in
        updatedWorld


platformsNear : Float2 -> World -> List { a : Platform, b : Rectangle, id : EntityID }
platformsNear ( dx, dy ) world =
    let
        playerLocs =
            world
                &. entities2 player transforms
                |> List.map .b

        platformEnts =
            world
                &. entities2 platforms transforms

        near playerSweep ({ b } as platform) =
            if Math.collides playerSweep b then
                []
            else
                [ platform ]

        forPlayer playerLoc =
            List.foldl
                (\platformEnt soFar ->
                    soFar
                        ++ near
                            { x = playerLoc.x + playerLoc.width / 2 - dx / 2
                            , y = playerLoc.y + playerLoc.height / 2 - dy / 2
                            , width = dx
                            , height = dy
                            }
                            platformEnt
                )
                []
                platformEnts
    in
        List.foldl (\playerEnt soFar -> soFar ++ forPlayer playerEnt) [] playerLocs


findPlaceInWorld : World -> Maybe ( ( Float, Float ), World )
findPlaceInWorld world =
    let
        findPlaceNear { b } =
            Pcg.step
                (Pcg.choices
                    ([ Pcg.constant
                        ( b.x - 16, b.y )
                     , Pcg.constant
                        ( b.x + 16, b.y )
                     , Pcg.constant
                        ( b.x, b.y - 16 )
                     , Pcg.constant
                        ( b.x, b.y + 16 )
                     ]
                    )
                )
                world.seed

        playerEnts =
            world &. (entities2 player transforms)

        thePlayer =
            List.head playerEnts
    in
        thePlayer
            |> Maybe.map findPlaceNear
            |> Maybe.map (\( location, updatedSeed ) -> ( location, { world | seed = updatedSeed } ))


spawns : List (Spawner World)
spawns =
    [ sparseSpawner
        { placer = findPlaceInWorld
        , checker = List.isEmpty << platformsNear ( 48, 48 )
        , spawner = spawnPlatform
        , chancer = (\_ -> 0)
        }
    ]
