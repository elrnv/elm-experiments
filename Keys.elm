module Keys exposing (Key(..), decode)

import Keyboard exposing (KeyCode)
import Dict exposing (Dict)

type Key
    = Cancel
    | Help
    | BackSpace
    | Tab
    | Clear
    | Enter
    | Shift
    | Control
    | Alt
    | Pause
    | CapsLock
    | Escape
    | Convert
    | NonConvert
    | Accept
    | ModeChange
    | Space
    | PageUp
    | PageDown
    | End
    | Home
    | ArrowLeft
    | ArrowUp
    | ArrowRight
    | ArrowDown
    | Select
    | Print
    | Execute
    | PrintScreen
    | Insert
    | Delete
    | Number0
    | Number1
    | Number2
    | Number3
    | Number4
    | Number5
    | Number6
    | Number7
    | Number8
    | Number9
    | Colon
    | Semicolon
    | LessThan
    | Equals
    | GreaterThan
    | QuestionMark
    | At
    | CharA
    | CharB
    | CharC
    | CharD
    | CharE
    | CharF
    | CharG
    | CharH
    | CharI
    | CharJ
    | CharK
    | CharL
    | CharM
    | CharN
    | CharO
    | CharP
    | CharQ
    | CharR
    | CharS
    | CharT
    | CharU
    | CharV
    | CharW
    | CharX
    | CharY
    | CharZ
    | Super
    | ContextMenu
    | Sleep
    | Numpad0
    | Numpad1
    | Numpad2
    | Numpad3
    | Numpad4
    | Numpad5
    | Numpad6
    | Numpad7
    | Numpad8
    | Numpad9
    | Multiply
    | Add
    | Separator
    | Subtract
    | Decimal
    | Divide
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | F13
    | F14
    | F15
    | F16
    | F17
    | F18
    | F19
    | F20
    | F21
    | F22
    | F23
    | F24
    | NumLock
    | ScrollLock
    | Circumflex
    | Exclamation
    | DoubleQuote
    | Hash
    | Dollar
    | Percent
    | Ampersand
    | Underscore
    | OpenParen
    | CloseParen
    | Asterisk
    | Plus
    | Pipe
    | HyphenMinus
    | OpenCurlyBracket
    | CloseCurlyBracket
    | Tilde
    | VolumeMute
    | VolumeDown
    | VolumeUp
    | Comma
    | Minus
    | Period
    | Slash
    | BackQuote
    | OpenBracket
    | BackSlash
    | CloseBracket
    | Quote
    | Meta
    | Altgr
    | Other


decode : KeyCode -> Maybe Key
decode code = Dict.get code codeDict

codeDict : Dict KeyCode Key
codeDict =
    Dict.fromList codeList


codeList : List ( KeyCode, Key )
codeList =
    [ ( 3, Cancel )
    , ( 6, Help )
    , ( 8, BackSpace )
    , ( 9, Tab )
    , ( 12, Clear )
    , ( 13, Enter )
    , ( 16, Shift )
    , ( 17, Control )
    , ( 18, Alt )
    , ( 19, Pause )
    , ( 20, CapsLock )
    , ( 27, Escape )
    , ( 28, Convert )
    , ( 29, NonConvert )
    , ( 30, Accept )
    , ( 31, ModeChange )
    , ( 32, Space )
    , ( 33, PageUp )
    , ( 34, PageDown )
    , ( 35, End )
    , ( 36, Home )
    , ( 37, ArrowLeft )
    , ( 38, ArrowUp )
    , ( 39, ArrowRight )
    , ( 40, ArrowDown )
    , ( 41, Select )
    , ( 42, Print )
    , ( 43, Execute )
    , ( 44, PrintScreen )
    , ( 45, Insert )
    , ( 46, Delete )
    , ( 48, Number0 )
    , ( 49, Number1 )
    , ( 50, Number2 )
    , ( 51, Number3 )
    , ( 52, Number4 )
    , ( 53, Number5 )
    , ( 54, Number6 )
    , ( 55, Number7 )
    , ( 56, Number8 )
    , ( 57, Number9 )
    , ( 58, Colon )
    , ( 59, Semicolon )
    , ( 60, LessThan )
    , ( 61, Equals )
    , ( 62, GreaterThan )
    , ( 63, QuestionMark )
    , ( 64, At )
    , ( 65, CharA )
    , ( 66, CharB )
    , ( 67, CharC )
    , ( 68, CharD )
    , ( 69, CharE )
    , ( 70, CharF )
    , ( 71, CharG )
    , ( 72, CharH )
    , ( 73, CharI )
    , ( 74, CharJ )
    , ( 75, CharK )
    , ( 76, CharL )
    , ( 77, CharM )
    , ( 78, CharN )
    , ( 79, CharO )
    , ( 80, CharP )
    , ( 81, CharQ )
    , ( 82, CharR )
    , ( 83, CharS )
    , ( 84, CharT )
    , ( 85, CharU )
    , ( 86, CharV )
    , ( 87, CharW )
    , ( 88, CharX )
    , ( 89, CharY )
    , ( 90, CharZ )
    , ( 91, Super )
    , ( 93, ContextMenu )
    , ( 95, Sleep )
    , ( 96, Numpad0 )
    , ( 97, Numpad1 )
    , ( 98, Numpad2 )
    , ( 99, Numpad3 )
    , ( 100, Numpad4 )
    , ( 101, Numpad5 )
    , ( 102, Numpad6 )
    , ( 103, Numpad7 )
    , ( 104, Numpad8 )
    , ( 105, Numpad9 )
    , ( 106, Multiply )
    , ( 107, Add )
    , ( 108, Separator )
    , ( 109, Subtract )
    , ( 110, Decimal )
    , ( 111, Divide )
    , ( 112, F1 )
    , ( 113, F2 )
    , ( 114, F3 )
    , ( 115, F4 )
    , ( 116, F5 )
    , ( 117, F6 )
    , ( 118, F7 )
    , ( 119, F8 )
    , ( 120, F9 )
    , ( 121, F10 )
    , ( 122, F11 )
    , ( 123, F12 )
    , ( 124, F13 )
    , ( 125, F14 )
    , ( 126, F15 )
    , ( 127, F16 )
    , ( 128, F17 )
    , ( 129, F18 )
    , ( 130, F19 )
    , ( 131, F20 )
    , ( 132, F21 )
    , ( 133, F22 )
    , ( 134, F23 )
    , ( 135, F24 )
    , ( 144, NumLock )
    , ( 145, ScrollLock )
    , ( 160, Circumflex )
    , ( 161, Exclamation )
    , ( 162, DoubleQuote )
    , ( 163, Hash )
    , ( 164, Dollar )
    , ( 165, Percent )
    , ( 166, Ampersand )
    , ( 167, Underscore )
    , ( 168, OpenParen )
    , ( 169, CloseParen )
    , ( 170, Asterisk )
    , ( 171, Plus )
    , ( 172, Pipe )
    , ( 173, HyphenMinus )
    , ( 174, OpenCurlyBracket )
    , ( 175, CloseCurlyBracket )
    , ( 176, Tilde )
    , ( 181, VolumeMute )
    , ( 182, VolumeDown )
    , ( 183, VolumeUp )
    , ( 186, Semicolon )
    , ( 187, Equals )
    , ( 188, Comma )
    , ( 189, Minus )
    , ( 190, Period )
    , ( 191, Slash )
    , ( 192, BackQuote )
    , ( 219, OpenBracket )
    , ( 220, BackSlash )
    , ( 221, CloseBracket )
    , ( 222, Quote )
    , ( 224, Meta )
    , ( 225, Altgr )
    ]
