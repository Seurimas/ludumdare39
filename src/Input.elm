module Input exposing (..)

import Mouse exposing (Position, moves)
import Keyboard exposing (KeyCode)


type Msg
    = MouseDown Position
    | MouseUp Position
    | MouseMove Position
    | KeyUp KeyCode
    | KeyDown KeyCode


type alias MouseState =
    { mousePlace : Position
    , mouseDown : Bool
    }


type alias KeyState =
    { left : Bool
    , right : Bool
    , down : Bool
    , up : Bool
    }


type alias InputState =
    { mouseState : MouseState
    , keyState : KeyState
    }


initInptState =
    { mouseState =
        { mousePlace = { x = 0, y = 0 }
        , mouseDown = False
        }
    , keyState =
        { left = false
        , right = false
        , up = false
        , down = false
        }
    }


subs =
    Subs.batch
        [ moves MouseMove
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
