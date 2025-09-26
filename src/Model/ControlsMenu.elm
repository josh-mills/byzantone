module Model.ControlsMenu exposing
    ( OpenControlMenus, init
    , MenuOption(..), menuOptions
    , anyOpen, isOpen, toggle
    )

{-|

@docs OpenControlMenus, init

@docs MenuOption, menuOptions

@docs anyOpen, isOpen, toggle

For desktop, we'll allow for multiple open menus.
For mobile, only one at a time.

-}


type MenuOption
    = AudioModeMenu
    | AudioSettingsMenu
    | IsonMenu
    | ScaleMenu
    | VolumeMenu


menuOptions : List MenuOption
menuOptions =
    [ AudioModeMenu, AudioSettingsMenu, IsonMenu, ScaleMenu, VolumeMenu ]


type alias OpenControlMenus =
    { audioModeIsOpen : Bool
    , audioSettingsMenuIsOpen : Bool
    , isonMenuIsOpen : Bool
    , scaleMenuIsOpen : Bool
    , volumeMenuIsOpen : Bool
    }


init : OpenControlMenus
init =
    { audioModeIsOpen = False
    , audioSettingsMenuIsOpen = False
    , isonMenuIsOpen = False
    , scaleMenuIsOpen = False
    , volumeMenuIsOpen = False
    }


anyOpen : OpenControlMenus -> Bool
anyOpen openControlMenus =
    List.any (isOpen openControlMenus) menuOptions


isOpen : OpenControlMenus -> MenuOption -> Bool
isOpen openControlMenus menuOption =
    case menuOption of
        AudioModeMenu ->
            openControlMenus.audioModeIsOpen

        AudioSettingsMenu ->
            openControlMenus.audioSettingsMenuIsOpen

        IsonMenu ->
            openControlMenus.isonMenuIsOpen

        ScaleMenu ->
            openControlMenus.scaleMenuIsOpen

        VolumeMenu ->
            openControlMenus.volumeMenuIsOpen


toggle : MenuOption -> OpenControlMenus -> OpenControlMenus
toggle controlMenu openControlMenus =
    case controlMenu of
        AudioModeMenu ->
            { openControlMenus | audioModeIsOpen = not openControlMenus.audioModeIsOpen }

        AudioSettingsMenu ->
            { openControlMenus | audioSettingsMenuIsOpen = not openControlMenus.audioSettingsMenuIsOpen }

        IsonMenu ->
            { openControlMenus | isonMenuIsOpen = not openControlMenus.isonMenuIsOpen }

        ScaleMenu ->
            { openControlMenus | scaleMenuIsOpen = not openControlMenus.scaleMenuIsOpen }

        VolumeMenu ->
            { openControlMenus | volumeMenuIsOpen = not openControlMenus.volumeMenuIsOpen }
