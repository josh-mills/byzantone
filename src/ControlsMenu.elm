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
    | ScaleMenu
    | VolumeMenu


menuOptions : List MenuOption
menuOptions =
    [ AudioModeMenu, ScaleMenu, VolumeMenu ]


type alias OpenControlMenus =
    { audioModeIsOpen : Bool
    , scaleMenuIsOpen : Bool
    , volumeMenuIsOpen : Bool
    }


init : OpenControlMenus
init =
    { audioModeIsOpen = False
    , scaleMenuIsOpen = False
    , volumeMenuIsOpen = False
    }


isOpen : OpenControlMenus -> MenuOption -> Bool
isOpen openControlMenus menuOption =
    case menuOption of
        AudioModeMenu ->
            openControlMenus.audioModeIsOpen

        ScaleMenu ->
            openControlMenus.scaleMenuIsOpen

        VolumeMenu ->
            openControlMenus.volumeMenuIsOpen


toggle : MenuOption -> OpenControlMenus -> OpenControlMenus
toggle controlMenu openControlMenus =
    case controlMenu of
        AudioModeMenu ->
            { openControlMenus | audioModeIsOpen = not openControlMenus.audioModeIsOpen }

        ScaleMenu ->
            { openControlMenus | scaleMenuIsOpen = not openControlMenus.scaleMenuIsOpen }

        VolumeMenu ->
            { openControlMenus | volumeMenuIsOpen = not openControlMenus.volumeMenuIsOpen }
