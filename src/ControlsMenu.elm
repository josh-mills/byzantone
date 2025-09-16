module ControlsMenu exposing
    ( OpenControlMenus, init
    , MenuOption(..), menuOptions
    , isOpen, toggle
    )

{-|

@docs OpenControlMenus, init

@docs MenuOption, menuOptions

@docs isOpen, toggle

For desktop, we'll allow for multiple open menus.
For mobile, only one at a time.

-}


type MenuOption
    = AudioModeMenu
    | VolumeMenu


menuOptions : List MenuOption
menuOptions =
    [ AudioModeMenu, VolumeMenu ]


type alias OpenControlMenus =
    { audioModeIsOpen : Bool
    , volumeMenuIsOpen : Bool
    }


init : OpenControlMenus
init =
    { audioModeIsOpen = False
    , volumeMenuIsOpen = False
    }


isOpen : OpenControlMenus -> MenuOption -> Bool
isOpen openControlMenus menuOption =
    case menuOption of
        AudioModeMenu ->
            openControlMenus.audioModeIsOpen

        VolumeMenu ->
            openControlMenus.volumeMenuIsOpen


toggle : MenuOption -> OpenControlMenus -> OpenControlMenus
toggle controlMenu openControlMenus =
    case controlMenu of
        AudioModeMenu ->
            { openControlMenus | audioModeIsOpen = not openControlMenus.audioModeIsOpen }

        VolumeMenu ->
            { openControlMenus | volumeMenuIsOpen = not openControlMenus.volumeMenuIsOpen }
