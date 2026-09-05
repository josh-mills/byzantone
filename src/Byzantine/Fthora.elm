module Byzantine.Fthora exposing (..)

import Byzantine.Degree exposing (Degree(..))
import Byzantine.Scale exposing (Scale(..))


type Fthora
    = DiatonicFthora DiatonicDegree
    | EnharmonicFthora EnharmonicDegree
    | SoftChromaticFthora SoftChromaticDegree
    | HardChromaticFthora HardChromaticDegree


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


{-| Construct the fthora for a given scale and degree, if one exists
(not every degree has a fthora for every position).
-}
for : Scale -> Degree -> Maybe Fthora
for scale degree =
    case scale of
        Diatonic ->
            case degree of
                Ni ->
                    Just (DiatonicFthora D_Ni)

                Pa ->
                    Just (DiatonicFthora D_Pa)

                Bou ->
                    Just (DiatonicFthora D_Bou)

                Ga ->
                    Just (DiatonicFthora D_Ga)

                Di ->
                    Just (DiatonicFthora D_Di)

                Ke ->
                    Just (DiatonicFthora D_Ke)

                Zo_ ->
                    Just (DiatonicFthora D_Zo_)

                Ni_ ->
                    Just (DiatonicFthora D_Ni_)

                _ ->
                    Nothing

        Enharmonic ->
            case degree of
                Zo_ ->
                    Just (EnharmonicFthora E_Zo_)

                _ ->
                    Nothing

        SoftChromatic ->
            case degree of
                Di ->
                    Just (SoftChromaticFthora SC_Di)

                Ke ->
                    Just (SoftChromaticFthora SC_Ke)

                _ ->
                    Nothing

        HardChromatic ->
            case degree of
                Pa ->
                    Just (HardChromaticFthora HC_Pa)

                Di ->
                    Just (HardChromaticFthora HC_Di)

                _ ->
                    Nothing
