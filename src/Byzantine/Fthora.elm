module Byzantine.Fthora exposing (..)


type Fthora
    = Diatonic DiatonicDegree
    | Enharmonic EnharmonicDegree
    | SoftChromatic SoftChromaticDegree
    | HardChromatic HardChromaticDegree


type DiatonicDegree
    = D_Ni
    | D_Pa
    | D_Bou
    | D_Ga
    | D_Di
    | D_Ke
    | D_Zo_
    | D_Ni_


type EnharmonicDegree
    = E_Zo_


type HardChromaticDegree
    = HC_Pa
    | HC_Di


type SoftChromaticDegree
    = SC_Di
    | SC_Ke
