module Assets.Reference exposing (..)

import Whistle.Native
import Assets.Loading exposing (Assets)
import Whistle.Types exposing (RawNode, Buffer)
import Whistle
import Task
import WebGL.Texture exposing (Texture)
import Vector2 exposing (Float2, scale, add)
import Vector3 exposing (Float3)


type Sfx
    = Fizzle
    | Fire
    | NewSpell


type Sprite
    = Player
    | ProjFire
    | ProjIce
    | ProjMagic
    | ProjFizzle
    | SpellFire
    | SpellIce
    | SpellMagic
    | SpellFizzle
    | Goblin
    | Hobgoblin
    | Hulk


sheetSize =
    512


bottomLeft sprite =
    case sprite of
        Player ->
            ( 168, sheetSize - 56 )

        ProjFire ->
            ( 128, sheetSize - 46 )

        ProjIce ->
            ( 136, sheetSize - 46 )

        ProjMagic ->
            ( 144, sheetSize - 46 )

        ProjFizzle ->
            ( 152, sheetSize - 46 )

        SpellFire ->
            ( 128, sheetSize - 32 )

        SpellIce ->
            ( 160, sheetSize - 32 )

        SpellMagic ->
            ( 192, sheetSize - 32 )

        SpellFizzle ->
            ( 224, sheetSize - 32 )

        Goblin ->
            ( 128, sheetSize - 80 )

        Hobgoblin ->
            ( 128, sheetSize - 97 )

        Hulk ->
            ( 192, sheetSize - 54 )


size sprite =
    case sprite of
        Player ->
            ( 16, 16 )

        ProjFire ->
            ( 8, 14 )

        ProjIce ->
            ( 8, 14 )

        ProjMagic ->
            ( 8, 14 )

        ProjFizzle ->
            ( 8, 14 )

        Goblin ->
            ( 48, 16 )

        Hobgoblin ->
            ( 48, 16 )

        Hulk ->
            ( 60, 21 )

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


getSfx : Sfx -> Maybe Assets -> Buffer
getSfx sfx assets_ =
    case assets_ of
        Just assets ->
            case sfx of
                Fizzle ->
                    assets.fizzleWav

                Fire ->
                    assets.fireWav

                NewSpell ->
                    assets.newSpellWav

        Nothing ->
            Debug.crash "No assets"


playSfx : (RawNode -> msg) -> Sfx -> Float -> Maybe Assets -> Cmd msg
playSfx playCmd sfx volume assets =
    Task.perform playCmd
        (Task.sequence
            [ Whistle.Native.createBufferSource False (getSfx sfx assets)
                |> Task.andThen Whistle.Native.startSourceNow
            , Whistle.Native.createGainNode volume
            ]
            |> Task.andThen Whistle.linkNodes
            |> Task.andThen Whistle.linkToOutput
        )
