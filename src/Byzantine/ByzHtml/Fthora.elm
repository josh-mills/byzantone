module Byzantine.ByzHtml.Fthora exposing
    ( diatonicNiLowAbove, diatonicNiLowSecondary, diatonicNiLowTertiary, diatonicNiLowBelow
    , diatonicPaAbove, diatonicPaSecondary, diatonicPaTertiary, diatonicPaBelow
    , diatonicVouAbove, diatonicVouSecondary, diatonicVouTertiary, diatonicVouBelow
    , diatonicGaAbove, diatonicGaSecondary, diatonicGaTertiary, diatonicGaBelow
    , diatonicDiAbove, diatonicDiSecondary, diatonicDiTertiary, diatonicDiBelow
    , diatonicKeAbove, diatonicKeSecondary, diatonicKeTertiary, diatonicKeBelow
    , diatonicZoAbove, diatonicZoSecondary, diatonicZoTertiary, diatonicZoBelow
    , diatonicNiHighAbove, diatonicNiHighSecondary, diatonicNiHighTertiary, diatonicNiHighBelow
    , hardChromaticPaAbove, hardChromaticPaSecondary, hardChromaticPaTertiary, hardChromaticPaBelow
    , hardChromaticDiAbove, hardChromaticDiSecondary, hardChromaticDiTertiary, hardChromaticDiBelow
    , softChromaticDiAbove, softChromaticDiSecondary, softChromaticDiTertiary, softChromaticDiBelow
    , softChromaticKeAbove, softChromaticKeSecondary, softChromaticKeTertiary, softChromaticKeBelow
    , enharmonicAbove, enharmonicSecondary, enharmonicTertiary, enharmonicBelow
    , agemAbove, agemSecondary, agemTertiary, agemBelow
    , zygosAbove, zygosSecondary, zygosTertiary, zygosBelow
    , klitonAbove, klitonSecondary, klitonTertiary, klitonBelow
    , spathiAbove, spathiSecondary, spathiTertiary, spathiBelow
    )

{-|


## Diatonic Ni Low

@docs diatonicNiLowAbove, diatonicNiLowSecondary, diatonicNiLowTertiary, diatonicNiLowBelow


## Diatonic Pa

@docs diatonicPaAbove, diatonicPaSecondary, diatonicPaTertiary, diatonicPaBelow


## Diatonic Vou

@docs diatonicVouAbove, diatonicVouSecondary, diatonicVouTertiary, diatonicVouBelow


## Diatonic Ga

@docs diatonicGaAbove, diatonicGaSecondary, diatonicGaTertiary, diatonicGaBelow


## Diatonic Di

@docs diatonicDiAbove, diatonicDiSecondary, diatonicDiTertiary, diatonicDiBelow


## Diatonic Ke

@docs diatonicKeAbove, diatonicKeSecondary, diatonicKeTertiary, diatonicKeBelow


## Diatonic Zo

@docs diatonicZoAbove, diatonicZoSecondary, diatonicZoTertiary, diatonicZoBelow


## Diatonic Ni High

@docs diatonicNiHighAbove, diatonicNiHighSecondary, diatonicNiHighTertiary, diatonicNiHighBelow


## Hard Chromatic Pa

@docs hardChromaticPaAbove, hardChromaticPaSecondary, hardChromaticPaTertiary, hardChromaticPaBelow


## Hard Chromatic Di

@docs hardChromaticDiAbove, hardChromaticDiSecondary, hardChromaticDiTertiary, hardChromaticDiBelow


## Soft Chromatic Di

@docs softChromaticDiAbove, softChromaticDiSecondary, softChromaticDiTertiary, softChromaticDiBelow


## Soft Chromatic Ke

@docs softChromaticKeAbove, softChromaticKeSecondary, softChromaticKeTertiary, softChromaticKeBelow


## Enharmonic

@docs enharmonicAbove, enharmonicSecondary, enharmonicTertiary, enharmonicBelow


## Agem

@docs agemAbove, agemSecondary, agemTertiary, agemBelow


## Zygos

@docs zygosAbove, zygosSecondary, zygosTertiary, zygosBelow


## Kliton

@docs klitonAbove, klitonSecondary, klitonTertiary, klitonBelow


## Spathi

@docs spathiAbove, spathiSecondary, spathiTertiary, spathiBelow

-}

import Html exposing (Html)



-- diatonic ni low


diatonicNiLowAbove : Html msg
diatonicNiLowAbove =
    Html.node "x-fthora-diatonic-ni-low-above" [] []


diatonicNiLowSecondary : Html msg
diatonicNiLowSecondary =
    Html.node "x-fthora-diatonic-ni-low-secondary" [] []


diatonicNiLowTertiary : Html msg
diatonicNiLowTertiary =
    Html.node "x-fthora-diatonic-ni-low-tertiary" [] []


diatonicNiLowBelow : Html msg
diatonicNiLowBelow =
    Html.node "x-fthora-diatonic-ni-low-below" [] []



-- diatonic pa


diatonicPaAbove : Html msg
diatonicPaAbove =
    Html.node "x-fthora-diatonic-pa-above" [] []


diatonicPaSecondary : Html msg
diatonicPaSecondary =
    Html.node "x-fthora-diatonic-pa-secondary" [] []


diatonicPaTertiary : Html msg
diatonicPaTertiary =
    Html.node "x-fthora-diatonic-pa-tertiary" [] []


diatonicPaBelow : Html msg
diatonicPaBelow =
    Html.node "x-fthora-diatonic-pa-below" [] []



-- diatonic vou


diatonicVouAbove : Html msg
diatonicVouAbove =
    Html.node "x-fthora-diatonic-vou-above" [] []


diatonicVouSecondary : Html msg
diatonicVouSecondary =
    Html.node "x-fthora-diatonic-vou-secondary" [] []


diatonicVouTertiary : Html msg
diatonicVouTertiary =
    Html.node "x-fthora-diatonic-vou-tertiary" [] []


diatonicVouBelow : Html msg
diatonicVouBelow =
    Html.node "x-fthora-diatonic-vou-below" [] []



-- diatonic ga


diatonicGaAbove : Html msg
diatonicGaAbove =
    Html.node "x-fthora-diatonic-ga-above" [] []


diatonicGaSecondary : Html msg
diatonicGaSecondary =
    Html.node "x-fthora-diatonic-ga-secondary" [] []


diatonicGaTertiary : Html msg
diatonicGaTertiary =
    Html.node "x-fthora-diatonic-ga-tertiary" [] []


diatonicGaBelow : Html msg
diatonicGaBelow =
    Html.node "x-fthora-diatonic-ga-below" [] []



-- diatonic di


diatonicDiAbove : Html msg
diatonicDiAbove =
    Html.node "x-fthora-diatonic-di-above" [] []


diatonicDiSecondary : Html msg
diatonicDiSecondary =
    Html.node "x-fthora-diatonic-di-secondary" [] []


diatonicDiTertiary : Html msg
diatonicDiTertiary =
    Html.node "x-fthora-diatonic-di-tertiary" [] []


diatonicDiBelow : Html msg
diatonicDiBelow =
    Html.node "x-fthora-diatonic-di-below" [] []



-- diatonic ke


diatonicKeAbove : Html msg
diatonicKeAbove =
    Html.node "x-fthora-diatonic-ke-above" [] []


diatonicKeSecondary : Html msg
diatonicKeSecondary =
    Html.node "x-fthora-diatonic-ke-secondary" [] []


diatonicKeTertiary : Html msg
diatonicKeTertiary =
    Html.node "x-fthora-diatonic-ke-tertiary" [] []


diatonicKeBelow : Html msg
diatonicKeBelow =
    Html.node "x-fthora-diatonic-ke-below" [] []



-- diatonic zo


diatonicZoAbove : Html msg
diatonicZoAbove =
    Html.node "x-fthora-diatonic-zo-above" [] []


diatonicZoSecondary : Html msg
diatonicZoSecondary =
    Html.node "x-fthora-diatonic-zo-secondary" [] []


diatonicZoTertiary : Html msg
diatonicZoTertiary =
    Html.node "x-fthora-diatonic-zo-tertiary" [] []


diatonicZoBelow : Html msg
diatonicZoBelow =
    Html.node "x-fthora-diatonic-zo-below" [] []



-- diatonic ni high


diatonicNiHighAbove : Html msg
diatonicNiHighAbove =
    Html.node "x-fthora-diatonic-ni-high-above" [] []


diatonicNiHighSecondary : Html msg
diatonicNiHighSecondary =
    Html.node "x-fthora-diatonic-ni-high-secondary" [] []


diatonicNiHighTertiary : Html msg
diatonicNiHighTertiary =
    Html.node "x-fthora-diatonic-ni-high-tertiary" [] []


diatonicNiHighBelow : Html msg
diatonicNiHighBelow =
    Html.node "x-fthora-diatonic-ni-high-below" [] []



-- hard chromatic pa


hardChromaticPaAbove : Html msg
hardChromaticPaAbove =
    Html.node "x-fthora-hard-chromatic-pa-above" [] []


hardChromaticPaSecondary : Html msg
hardChromaticPaSecondary =
    Html.node "x-fthora-hard-chromatic-pa-secondary" [] []


hardChromaticPaTertiary : Html msg
hardChromaticPaTertiary =
    Html.node "x-fthora-hard-chromatic-pa-tertiary" [] []


hardChromaticPaBelow : Html msg
hardChromaticPaBelow =
    Html.node "x-fthora-hard-chromatic-pa-below" [] []



-- hard chromatic di


hardChromaticDiAbove : Html msg
hardChromaticDiAbove =
    Html.node "x-fthora-hard-chromatic-di-above" [] []


hardChromaticDiSecondary : Html msg
hardChromaticDiSecondary =
    Html.node "x-fthora-hard-chromatic-di-secondary" [] []


hardChromaticDiTertiary : Html msg
hardChromaticDiTertiary =
    Html.node "x-fthora-hard-chromatic-di-tertiary" [] []


hardChromaticDiBelow : Html msg
hardChromaticDiBelow =
    Html.node "x-fthora-hard-chromatic-di-below" [] []



-- soft chromatic di


softChromaticDiAbove : Html msg
softChromaticDiAbove =
    Html.node "x-fthora-soft-chromatic-di-above" [] []


softChromaticDiSecondary : Html msg
softChromaticDiSecondary =
    Html.node "x-fthora-soft-chromatic-di-secondary" [] []


softChromaticDiTertiary : Html msg
softChromaticDiTertiary =
    Html.node "x-fthora-soft-chromatic-di-tertiary" [] []


softChromaticDiBelow : Html msg
softChromaticDiBelow =
    Html.node "x-fthora-soft-chromatic-di-below" [] []



-- soft chromatic ke


softChromaticKeAbove : Html msg
softChromaticKeAbove =
    Html.node "x-fthora-soft-chromatic-ke-above" [] []


softChromaticKeSecondary : Html msg
softChromaticKeSecondary =
    Html.node "x-fthora-soft-chromatic-ke-secondary" [] []


softChromaticKeTertiary : Html msg
softChromaticKeTertiary =
    Html.node "x-fthora-soft-chromatic-ke-tertiary" [] []


softChromaticKeBelow : Html msg
softChromaticKeBelow =
    Html.node "x-fthora-soft-chromatic-ke-below" [] []



-- enharmonic


enharmonicAbove : Html msg
enharmonicAbove =
    Html.node "x-fthora-enharmonic-above" [] []


enharmonicSecondary : Html msg
enharmonicSecondary =
    Html.node "x-fthora-enharmonic-secondary" [] []


enharmonicTertiary : Html msg
enharmonicTertiary =
    Html.node "x-fthora-enharmonic-tertiary" [] []


enharmonicBelow : Html msg
enharmonicBelow =
    Html.node "x-fthora-enharmonic-below" [] []



-- agem


agemAbove : Html msg
agemAbove =
    Html.node "x-f-agem" [] []


agemSecondary : Html msg
agemSecondary =
    Html.node "x-f-agem-2" [] []


agemTertiary : Html msg
agemTertiary =
    Html.node "x-f-agem-3" [] []


agemBelow : Html msg
agemBelow =
    Html.node "x-f-agem-b" [] []



-- zygos


zygosAbove : Html msg
zygosAbove =
    Html.node "x-chroa-zygos-above" [] []


zygosSecondary : Html msg
zygosSecondary =
    Html.node "x-chroa-zygos-secondary" [] []


zygosTertiary : Html msg
zygosTertiary =
    Html.node "x-chroa-zygos-tertiary" [] []


zygosBelow : Html msg
zygosBelow =
    Html.node "x-chroa-zygos-below" [] []



-- kliton


klitonAbove : Html msg
klitonAbove =
    Html.node "x-chroa-kliton-above" [] []


klitonSecondary : Html msg
klitonSecondary =
    Html.node "x-chroa-kliton-secondary" [] []


klitonTertiary : Html msg
klitonTertiary =
    Html.node "x-chroa-kliton-tertiary" [] []


klitonBelow : Html msg
klitonBelow =
    Html.node "x-chroa-kliton-below" [] []



-- spathi


spathiAbove : Html msg
spathiAbove =
    Html.node "x-chroa-spathi-above" [] []


spathiSecondary : Html msg
spathiSecondary =
    Html.node "x-chroa-spathi-secondary" [] []


spathiTertiary : Html msg
spathiTertiary =
    Html.node "x-chroa-spathi-tertiary" [] []


spathiBelow : Html msg
spathiBelow =
    Html.node "x-chroa-spathi-below" [] []
