module Assets.Reference exposing (..)

import Whistle.Native
import Assets.Loading exposing (Assets)
import Whistle.Types exposing (RawNode, Buffer)
import Whistle
import Task
import WebGL.Texture exposing (Texture)
import Vector2 exposing (Float2, scale, add)
import Vector3 exposing (Float3)
import Dict


type Sfx
    = CastFizzle
    | Cast
    | NewSpell
    | HulkAttack
    | GoblinAttack
    | HobgoblinAttack
    | GameOverSfx


type Element
    = Fire
    | Ice
    | Magic
    | Fizzle
    | Chroma
    | FireMine
    | IceMine
    | HealthPack


type ExplosionType
    = Firey
    | Icey


type Sprite
    = Player
    | ProjSprite Element
    | SpellIcon Element
    | Goblin
    | Hobgoblin
    | Hulk
    | Platform
    | Explosion ExplosionType


sheetSize =
    512


explosionIndex explosion =
    case explosion of
        Firey ->
            0

        Icey ->
            1


elementIndex element =
    case element of
        Chroma ->
            0

        Fire ->
            1

        Ice ->
            2

        Magic ->
            3

        Fizzle ->
            4

        FireMine ->
            5

        IceMine ->
            6

        HealthPack ->
            7


bottomLeft sprite =
    case sprite of
        Player ->
            ( 104, sheetSize - 88 )

        ProjSprite element ->
            ( 120 + (elementIndex element) * 8, sheetSize - 46 )

        SpellIcon element ->
            ( 96 + (elementIndex element) * 32, sheetSize - 32 )

        Goblin ->
            ( 128, sheetSize - 80 )

        Hobgoblin ->
            ( 128, sheetSize - 97 )

        Hulk ->
            ( 192, sheetSize - 54 )

        Platform ->
            ( 0, sheetSize - 130 )

        Explosion explosion ->
            ( 176, sheetSize - 96 - 32 * (explosionIndex explosion) )


size sprite =
    case sprite of
        Player ->
            ( 16, 16 )

        ProjSprite _ ->
            ( 8, 14 )

        Goblin ->
            ( 48, 16 )

        Hobgoblin ->
            ( 48, 16 )

        Hulk ->
            ( 60, 21 )

        Platform ->
            ( 64, 67 )

        Explosion _ ->
            ( 128, 32 )

        _ ->
            ( 32, 32 )


numberOfFrames sprite =
    case sprite of
        Hobgoblin ->
            3

        Hulk ->
            3

        Goblin ->
            3

        Explosion _ ->
            4

        _ ->
            1


getSprite :
    Sprite
    -> Maybe Assets
    -> { texture : Maybe Texture
       , bottomLeft : Float2
       , topRight : Float2
       , numberOfFrames : Int
       , currentFrame : Int
       , position : Float3
       , rotation : Float
       , pivot : Float2
       , size : Float2
       , duration : Float
       }
getSprite sprite assets_ =
    let
        texture =
            assets_ |> Maybe.map .spriteMap

        bl =
            scale (1 / 512) (bottomLeft sprite)

        tr =
            scale (1 / 512) (size sprite)
                |> add bl

        pivot =
            ( 0.5, 0.5 )

        frames =
            (numberOfFrames sprite)
    in
        { texture = texture
        , topRight = tr
        , bottomLeft = bl
        , numberOfFrames = frames
        , currentFrame = 0
        , duration = 1
        , position = ( 0, 0, 0 )
        , rotation = 0
        , pivot = pivot
        , size = ( 1, 1 )
        }


getSfx : Sfx -> Assets -> Maybe Buffer
getSfx sfx assets =
    case sfx of
        CastFizzle ->
            Dict.get "fizzle.wav" assets.buffers

        Cast ->
            Dict.get "fire.wav" assets.buffers

        NewSpell ->
            Dict.get "newSpell.wav" assets.buffers

        GoblinAttack ->
            Dict.get "goblin.wav" assets.buffers

        HobgoblinAttack ->
            Dict.get "hobgoblin.wav" assets.buffers

        HulkAttack ->
            Dict.get "hulk.wav" assets.buffers

        GameOverSfx ->
            Dict.get "gameOver.wav" assets.buffers


playSfx : (RawNode -> msg) -> Sfx -> Float -> Maybe Assets -> Cmd msg
playSfx playCmd sfx volume assets =
    let
        mBuffer =
            (assets |> Maybe.andThen (getSfx sfx))
    in
        case mBuffer of
            Just buffer ->
                Task.perform playCmd
                    (Task.sequence
                        [ Whistle.Native.createBufferSource False buffer
                            |> Task.andThen Whistle.Native.startSourceNow
                        , Whistle.Native.createGainNode volume
                        ]
                        |> Task.andThen Whistle.linkNodes
                        |> Task.andThen Whistle.linkToOutput
                    )

            Nothing ->
                Cmd.none


playMusic : (RawNode -> msg) -> Maybe Assets -> Cmd msg
playMusic playCmd mAssets =
    let
        mBuffer =
            mAssets |> Maybe.andThen (\assets -> Dict.get "mainTheme.wav" assets.buffers)

        musicTask buffer =
            (Whistle.Native.createBufferSource True buffer
                |> Task.andThen Whistle.Native.startSourceNow
                |> Task.andThen Whistle.linkToOutput
            )
    in
        case mBuffer of
            Just buffer ->
                Task.perform playCmd (musicTask buffer)

            Nothing ->
                Cmd.none


stopMusic : (RawNode -> msg) -> Maybe RawNode -> Cmd msg
stopMusic stopCmd node =
    case node of
        Just bufferNode ->
            Task.perform stopCmd (Whistle.Native.stopSource bufferNode)

        Nothing ->
            Cmd.none
