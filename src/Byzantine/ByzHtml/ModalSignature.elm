module Byzantine.ByzHtml.ModalSignature exposing
    ( view
    , modePlagal, modeWordEchos
    , modeFirst, modeSecond, modeThird, modeThirdNana, modeFourth, modeLegetos, modePlagalFirst, modePlagalSecond, modeVarys, modeVarys2, modePlagalFourth, modeWordVarys
    , modeNi, modePa, modeVou, modeGa, modeDi, modeKe, modeZo
    , modeOligonKentimaAbove, modeOligonYpsili, modeElafron, modeRunningElafron
    , modeAlpha, modeBeta, modeGamma, modeDelta, modeAlphaCapital, modeBetaCapital, modeGammaCapital, modeDeltaCapital
    )

{-| WIP. The component elements are here, but the underlying type modeling needs
to be built out so we can implement a general `view` function.


# View

@docs view


# Components


## Division

@docs modePlagal, modeWordEchos


## Classification

@docs modeFirst, modeSecond, modeThird, modeThirdNana, modeFourth, modeLegetos, modePlagalFirst, modePlagalSecond, modeVarys, modeVarys2, modePlagalFourth, modeWordVarys


## Bases

@docs modeNi, modePa, modeVou, modeGa, modeDi, modeKe, modeZo


## Neumes

@docs modeOligonKentimaAbove, modeOligonYpsili, modeElafron, modeRunningElafron


## Ordinals

@docs modeAlpha, modeBeta, modeGamma, modeDelta, modeAlphaCapital, modeBetaCapital, modeGammaCapital, modeDeltaCapital

-}

import Byzantine.Mode.Classification as Classification exposing (Classification(..), Ordinal(..))
import Byzantine.Mode.Signature as Signature exposing (Elements, Ichos(..), Indicator(..), Signature)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Lazy



-- view


view : Signature -> Html msg
view modalSignature =
    Html.Lazy.lazy
        (\signature ->
            let
                elements : Elements
                elements =
                    Signature.elements signature
            in
            [ viewDivision elements.ichos
            , [ viewIndicator elements.indicator ]
            ]
                |> List.concat
                |> Html.div [ class "text-2xl" ]
        )
        modalSignature



{-
   viewMaybe : (a -> Html msg) -> Maybe a -> List (Html msg)
   viewMaybe f a =
       Maybe.Extra.unwrap [] (f >> List.singleton) a
-}
-- division


{-| Classification: Ichos or Ichos Plagal
-}
viewDivision : Ichos -> List (Html msg)
viewDivision ichos =
    case ichos of
        Ichos ->
            [ modeWordEchos ]

        IchosPlagal ->
            [ modeWordEchos, modePlagal ]


modePlagal : Html msg
modePlagal =
    Html.node "x-mode-plagal" [] []


modeWordEchos : Html msg
modeWordEchos =
    Html.node "x-mode-word-echos" [] []



-- indicators


viewIndicator : Indicator -> Html msg
viewIndicator indicator =
    case indicator of
        First ->
            modeFirst

        Second ->
            modeSecond

        Third ->
            modeThird

        ThirdNaNa ->
            modeThirdNana

        Fourth ->
            modeFourth

        Legetos ->
            modeLegetos

        PlagalFirst ->
            modePlagalFirst

        PlagalSecond ->
            modePlagalSecond

        Varys ->
            modeVarys

        VarysZo ->
            modeVarys2

        PlagalFourth ->
            modePlagalFourth

        ClassificationIndicator classification ->
            case Classification.ordinal classification of
                ModeOne ->
                    modeAlpha

                ModeTwo ->
                    modeBeta

                ModeThree ->
                    modeGamma

                ModeFour ->
                    modeDelta


modeFirst : Html msg
modeFirst =
    Html.node "x-mode-first" [] []


modeSecond : Html msg
modeSecond =
    Html.node "x-mode-second" [] []


modeThird : Html msg
modeThird =
    Html.node "x-mode-third" [] []


modeThirdNana : Html msg
modeThirdNana =
    Html.node "x-mode-third-nana" [] []


modeFourth : Html msg
modeFourth =
    Html.node "x-mode-fourth" [] []


modeLegetos : Html msg
modeLegetos =
    Html.node "x-mode-legetos" [] []


modePlagalFirst : Html msg
modePlagalFirst =
    Html.node "x-mode-plagal-first" [] []


modePlagalSecond : Html msg
modePlagalSecond =
    Html.node "x-mode-plagal-second" [] []


modeVarys : Html msg
modeVarys =
    Html.node "x-mode-varys" [] []


modeVarys2 : Html msg
modeVarys2 =
    Html.node "x-mode-varys2" [] []


modePlagalFourth : Html msg
modePlagalFourth =
    Html.node "x-mode-plagal-fourth" [] []


modeWordVarys : Html msg
modeWordVarys =
    Html.node "x-mode-word-varys" [] []



-- bases


modeNi : Html msg
modeNi =
    Html.node "x-mode-ni" [] []


modePa : Html msg
modePa =
    Html.node "x-mode-pa" [] []


modeVou : Html msg
modeVou =
    Html.node "x-mode-vou" [] []


modeGa : Html msg
modeGa =
    Html.node "x-mode-ga" [] []


modeDi : Html msg
modeDi =
    Html.node "x-mode-di" [] []


modeKe : Html msg
modeKe =
    Html.node "x-mode-ke" [] []


modeZo : Html msg
modeZo =
    Html.node "x-mode-zo" [] []



-- neumes


modeOligonKentimaAbove : Html msg
modeOligonKentimaAbove =
    Html.node "x-mode-oligon-kentima-above" [] []


modeOligonYpsili : Html msg
modeOligonYpsili =
    Html.node "x-mode-oligon-ypsili" [] []


modeElafron : Html msg
modeElafron =
    Html.node "x-mode-elafron" [] []


modeRunningElafron : Html msg
modeRunningElafron =
    Html.node "x-mode-running-elafron" [] []



-- classification ordinals


modeAlpha : Html msg
modeAlpha =
    Html.node "x-mode-alpha" [] []


modeBeta : Html msg
modeBeta =
    Html.node "x-mode-beta" [] []


modeGamma : Html msg
modeGamma =
    Html.node "x-mode-gamma" [] []


modeDelta : Html msg
modeDelta =
    Html.node "x-mode-delta" [] []


modeAlphaCapital : Html msg
modeAlphaCapital =
    Html.node "x-mode-alpha-capital" [] []


modeBetaCapital : Html msg
modeBetaCapital =
    Html.node "x-mode-beta-capital" [] []


modeGammaCapital : Html msg
modeGammaCapital =
    Html.node "x-mode-gamma-capital" [] []


modeDeltaCapital : Html msg
modeDeltaCapital =
    Html.node "x-mode-delta-capital" [] []
