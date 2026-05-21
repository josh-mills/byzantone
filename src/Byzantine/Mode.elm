module Byzantine.Mode exposing
    ( Mode(..), toString, all
    , ModeData, data
    , Inflection
    )

{-| Data structure to capture modal characteristics.


# Mode

@docs Mode, toString, all


# Mode Data

@docs ModeData, data


## Melodic Attractions

@docs Inflection

-}

import Byzantine.Accidental exposing (Accidental(..))
import Byzantine.Degree exposing (Degree(..))
import Byzantine.Mode.Classification exposing (Classification(..), Ordinal(..))
import Byzantine.Mode.Genre exposing (Genre(..))
import Byzantine.Scale exposing (Scale(..))


type Mode
    = AuthenticOne_Ke_Papadic
    | AuthenticOne_Pa_Eirmologic
    | AuthenticOne_Pa_Sticheraric
    | AuthenticTwo_Di
    | AuthenticThree_Ga_Eirmologic
    | AuthenticThree_Ga_Papadic
    | AuthenticFour_Bou_Legetos
    | AuthenticFour_Di_Agia
    | AuthenticFour_Pa_Sticheraric
    | PlagalOne_Pa_Sticheraric
    | PlagalOne_Pa_Papadic
    | PlagalOne_Ke_Eirmologic
    | PlagalOne_Pa_Pentaphone
    | PlagalOne_Pa_Phrygian
    | PlagalOne_Pa_Minor
    | PlagalTwo_Pa
    | PlagalThree_Ga
    | PlagalThree_Zo
    | PlagalThree_Zo_Hard
    | PlagalFour_Ni
    | PlagalFour_Ga


{-| Display string.
-}
toString : Mode -> String
toString mode =
    case mode of
        AuthenticOne_Ke_Papadic ->
            "Authentic Mode One, Papadic"

        AuthenticOne_Pa_Eirmologic ->
            "Lower Mode One, Eirmologic"

        AuthenticOne_Pa_Sticheraric ->
            "Lower Mode One, Sticheraric"

        AuthenticTwo_Di ->
            "Authentic Mode Two, Soft Chromatic"

        AuthenticThree_Ga_Eirmologic ->
            "Authentic Mode Three, Eirmologic"

        AuthenticThree_Ga_Papadic ->
            "Authentic Mode Three, Papadic"

        AuthenticFour_Bou_Legetos ->
            "Legetos, Medial Fourth Mode (Eirmologic)"

        AuthenticFour_Di_Agia ->
            "Agia, Authentic Fourth Mode (Papadic)"

        AuthenticFour_Pa_Sticheraric ->
            "Fourth-First, Sticheraric Fourth Mode"

        PlagalOne_Pa_Sticheraric ->
            "Plagal Mode One, Sticheraric"

        PlagalOne_Pa_Papadic ->
            "Plagal Mode One, Papadic"

        PlagalOne_Ke_Eirmologic ->
            "Plagal Mode One, Eirmologic"

        PlagalOne_Pa_Pentaphone ->
            "Plagal Mode One, Hard-Diatonic Pentaphone"

        PlagalOne_Pa_Phrygian ->
            "Plagal Mode One, Hard-Diatonic Phrygian"

        PlagalOne_Pa_Minor ->
            "Plagal Mode One, Hard-Diatonic Minor"

        PlagalTwo_Pa ->
            "Plagal Mode Two, Hard Chromatic"

        PlagalThree_Ga ->
            "Grave Mode (Eirmologic, Sticheraric)"

        PlagalThree_Zo ->
            "Soft-Diatonic Grave Mode on Zo"

        PlagalThree_Zo_Hard ->
            "Hard-Diatonic Grave Mode on Zo-flat"

        PlagalFour_Ni ->
            "Plagal Mode Four, Neagie"

        PlagalFour_Ga ->
            "Triphone Neagie, Plagal Mode Four on Ga"


all : List Mode
all =
    [ AuthenticOne_Ke_Papadic
    , AuthenticOne_Pa_Eirmologic
    , AuthenticOne_Pa_Sticheraric
    , AuthenticTwo_Di
    , AuthenticThree_Ga_Eirmologic
    , AuthenticThree_Ga_Papadic
    , AuthenticFour_Bou_Legetos
    , AuthenticFour_Di_Agia
    , AuthenticFour_Pa_Sticheraric
    , PlagalOne_Pa_Sticheraric
    , PlagalOne_Pa_Papadic
    , PlagalOne_Ke_Eirmologic
    , PlagalOne_Pa_Pentaphone
    , PlagalOne_Pa_Phrygian
    , PlagalOne_Pa_Minor
    , PlagalTwo_Pa
    , PlagalThree_Ga
    , PlagalThree_Zo
    , PlagalThree_Zo_Hard
    , PlagalFour_Ni
    , PlagalFour_Ga
    ]



-- MODE DATA


data : Mode -> ModeData
data mode =
    case mode of
        AuthenticOne_Ke_Papadic ->
            authenticOne_Ke_Papadic

        AuthenticOne_Pa_Eirmologic ->
            authenticOne_Pa_Eirmologic

        AuthenticOne_Pa_Sticheraric ->
            authenticOne_Pa_Sticheraric

        AuthenticTwo_Di ->
            authenticTwo_Di

        AuthenticThree_Ga_Eirmologic ->
            authenticThree_Ga_Eirmologic

        AuthenticThree_Ga_Papadic ->
            authenticThree_Ga_Papadic

        AuthenticFour_Bou_Legetos ->
            authenticFour_Bou_Legetos

        AuthenticFour_Di_Agia ->
            authenticFour_Di_Agia

        AuthenticFour_Pa_Sticheraric ->
            authenticFour_Pa_Sticheraric

        PlagalOne_Pa_Sticheraric ->
            plagalOne_Pa_Sticheraric

        PlagalOne_Pa_Papadic ->
            plagalOne_Pa_Papadic

        PlagalOne_Ke_Eirmologic ->
            plagalOne_Ke_Eirmologic

        PlagalOne_Pa_Pentaphone ->
            plagalOne_Pa_Pentaphone

        PlagalOne_Pa_Phrygian ->
            plagalOne_Pa_Phrygian

        PlagalOne_Pa_Minor ->
            plagalOne_Pa_Minor

        PlagalTwo_Pa ->
            plagalTwo_Pa

        PlagalThree_Ga ->
            plagalThree_Ga

        PlagalThree_Zo ->
            plagalThree_Zo

        PlagalThree_Zo_Hard ->
            plagalThree_Zo_Hard

        PlagalFour_Ni ->
            plagalFour_Ni

        PlagalFour_Ga ->
            plagalFour_Ga


{-| other points to capture:

  - ison (may also want to include "unison")
  - modal signature (may be more than one)
  - apichima

-}
type alias ModeData =
    { classification : Classification
    , genres : List Genre
    , scale : Scale
    , dominantTones : DominantTones
    , isonOptions : List Degree
    , range :
        { start : Degree
        , end : Degree
        }
    , recitingTone : Degree
    , possibleInflections : List Inflection
    }


type alias DominantTones =
    { base : Degree
    , cadencePoints : CadencePoints
    , nonCadentialFoci : List Degree
    }


type alias CadencePoints =
    { final : Degree
    , complete : List Degree
    , medial : List Degree
    , incomplete : List Degree
    }


{-| This will need to be expanded to capture additional musical details, namely,
context, and whether or not the inflection is discretionary or forced.
-}
type alias Inflection =
    { degree : Degree
    , accidentals : List Accidental
    }



-- CATALOG


authenticOne_Ke_Papadic : ModeData
authenticOne_Ke_Papadic =
    { classification = Authentic ModeOne
    , genres = [ Papadic ]
    , scale = Diatonic
    , dominantTones =
        { base = Ke
        , cadencePoints =
            { final = Ke
            , complete = [ Ke, Pa ]
            , medial = [ Ke, Ni_, Pa_ ]
            , incomplete = [ Ke, Pa_ ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Ke ]
    , range =
        { start = Pa
        , end = Pa_
        }
    , recitingTone = Ke
    , possibleInflections =
        [ { degree = Zo_, accidentals = [ Sharp2 ] }
        , { degree = Pa_, accidentals = [ Flat2, Flat4 ] }
        , { degree = Ni_, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Ga, accidentals = [ Sharp4, Sharp6 ] }
        ]
    }


authenticOne_Pa_Eirmologic : ModeData
authenticOne_Pa_Eirmologic =
    { classification = Authentic ModeOne
    , genres = [ Eirmologic ]
    , scale = Diatonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = [ Pa ]
            , medial = []
            , incomplete = [ Ga, Di ]
            }
        , nonCadentialFoci = [ Ke ]
        }
    , isonOptions = [ Pa, Di ]
    , range =
        { start = KE
        , end = Pa_
        }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Ga, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Bou, accidentals = [ Sharp2 ] }
        , { degree = Di, accidentals = [ Flat4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| Lower first, includes a papadic genre that has a few differences
with dominant tones.

Ke as non-cadential melodic focus

-}
authenticOne_Pa_Sticheraric : ModeData
authenticOne_Pa_Sticheraric =
    { classification = Authentic ModeOne
    , genres = [ Sticheraric ]
    , scale = Diatonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = [ Pa ]
            , medial = []
            , incomplete = [ Pa, Ga, Di ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, DI ]
    , range = { start = DI, end = Pa_ }
    , recitingTone = Pa
    , possibleInflections =
        [ { degree = Bou, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Di, accidentals = [ Flat2, Flat4, Flat6 ] }
        , { degree = Ga, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| A single mode entry covers all three performance genres, as the scale
structure and core attractions are identical. The minor distinctions between
eirmologic, sticheraric, and papadic practice lie in ornamentation and pacing
rather than modal content.

Dominant tone Ni "plagalizes" the mode and draws in the hard-chromatic phthora;
upper Ni at climactic moments shifts to a triphone, hard-diatonic organization.

  - TODO: The wiki's scale section body text says "Soft diatonic" while the
    section header says "Soft Chromatic". `SoftChromatic` was used to match the
    header; verify whether the body text is an error.
  - TODO: `medial = [Bou]` is inferred from the "melos/mediant" label. The wiki
    only explicitly assigns Bou to "complete and incomplete" cadences, not
    medial; verify whether Bou belongs here or only in `complete` and
    `incomplete`.

-}
authenticTwo_Di : ModeData
authenticTwo_Di =
    { classification = Authentic ModeTwo
    , genres = [ Eirmologic, Sticheraric, Papadic ]
    , scale = SoftChromatic
    , dominantTones =
        { base = Di
        , cadencePoints =
            { final = Di
            , complete = [ Di, Bou, Ni ]
            , medial = [ Bou ]
            , incomplete = [ Di, Bou, Zo_, Ni ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Di, Ni, DI ]
    , range = { start = Ni, end = Ga_ }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Ga, accidentals = [ Sharp2, Sharp4, Sharp6 ] }
        , { degree = Pa, accidentals = [ Sharp4, Sharp6 ] }
        , { degree = Ke, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Sharp2, Sharp4 ] }
        ]
    }


{-| Traditionally referred to simply as "Third Mode". The scale is formally
hard-diatonic, but is better understood as soft-diatonic with characteristic
ajém attractions. Strong connection to plagal first mode.

Ga is the base and final but is not typically used as a cadence point within
the melody; Pa and Ke carry the internal cadential weight.

  - TODO: The wiki explicitly flags scale ambiguity: "hard diatonic, but it may
    be better to think of this as soft diatonic with characteristic attractions".
    `Diatonic` was chosen for the theoretical foundation, but `Enharmonic` is
    equally defensible for the practical sound. Expert guidance needed.

-}
authenticThree_Ga_Eirmologic : ModeData
authenticThree_Ga_Eirmologic =
    { classification = Authentic ModeThree
    , genres = [ Eirmologic, Sticheraric ]
    , scale = Diatonic
    , dominantTones =
        { base = Ga
        , cadencePoints =
            { final = Ga
            , complete = [ Pa ]
            , medial = [ Ke ]
            , incomplete = [ Pa, Ke ]
            }
        , nonCadentialFoci = [ Di ]
        }
    , isonOptions = [ Ga, Pa, Ni, Ke ]
    , range = { start = Ni, end = Ga_ }
    , recitingTone = Ke
    , possibleInflections =
        [ { degree = Bou, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Di, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        , { degree = Pa, accidentals = [ Sharp4 ] }
        ]
    }


{-| The papadic form of third mode is inherently modulatory, freely
incorporating the dominant tones of grave mode (Plagal III), plagal fourth
(Neagie on Ni), and transposed first mode variants. The melodic attractions are
largely those of whichever modal region is currently active.

  - TODO: `recitingTone = Ke` is assumed by analogy with the eirmologic and
    sticheraric forms; JMB does not specify a reciting tone for the papadic form.

-}
authenticThree_Ga_Papadic : ModeData
authenticThree_Ga_Papadic =
    { classification = Authentic ModeThree
    , genres = [ Papadic ]
    , scale = Diatonic
    , dominantTones =
        { base = Ga
        , cadencePoints =
            { final = Ga
            , complete = [ Pa ]
            , medial = []
            , incomplete = [ Pa, Ni, Di, Ke ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Ga, Pa ]
    , range = { start = Ni, end = Ga_ }
    , recitingTone = Ke
    , possibleInflections =
        [ { degree = Ke, accidentals = [ Sharp2 ] }
        , { degree = Bou_, accidentals = [ Flat4 ] }
        ]
    }


{-| Medial fourth mode, halfway between Agia on Di and Neagie on Ni. The
position of Ke is context-sensitive: lowered by two units to form a true
tetrachord with Bou when approaching a Bou cadence, or raised by four units
when passing to a natural Zo or into an Agia phrase.

  - TODO: `{ degree = Ke, accidentals = [ Flat2, Sharp4 ] }` conflates two
    structurally distinct behaviors: the `Flat2` is a scale-level adjustment
    that redefines the tetrachord (not an ornamental inflection), while `Sharp4`
    is an independent ornamental attraction toward Zo. These should be modeled
    differently once the `Inflection` type is extended to capture context.

-}
authenticFour_Bou_Legetos : ModeData
authenticFour_Bou_Legetos =
    { classification = Authentic ModeFour
    , genres = [ Eirmologic ]
    , scale = Diatonic
    , dominantTones =
        { base = Bou
        , cadencePoints =
            { final = Bou
            , complete = [ Bou, Di ]
            , medial = []
            , incomplete = [ Bou, Di ]
            }
        , nonCadentialFoci = [ Zo_ ]
        }
    , isonOptions = [ Bou, Di, DI ]
    , range = { start = DI, end = Pa_ }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Pa, accidentals = [ Sharp2, Sharp4, Sharp6 ] }
        , { degree = Ga, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Ke, accidentals = [ Flat2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| The authentic fourth mode. Begins and ends on Di with Zo as the primary
reciting tone. Zo has a dual character: it remains natural when dominant, but
is flattened by four units when Di is dominant and the phrase descends.

  - TODO: `range.start = Zo` (lower octave, index 3). The wiki says "take Ζω as
    a typical lower limit" but does not clarify which octave. If this refers to
    Zo\_ (index 10, just above the base Di), the range would be substantially
    narrower; expert clarification needed.
  - TODO: `{ degree = Zo_, accidentals = [ Flat4, Sharp2, Sharp4 ] }` collapses
    two strictly context-dependent behaviors (flat when descending to a
    Di-dominant phrase; sharp when ascending toward Ni) into a single entry.
    Context cannot be represented in the current `Inflection` model.

-}
authenticFour_Di_Agia : ModeData
authenticFour_Di_Agia =
    { classification = Authentic ModeFour
    , genres = [ Papadic ]
    , scale = Diatonic
    , dominantTones =
        { base = Di
        , cadencePoints =
            { final = Di
            , complete = [ Di ]
            , medial = [ Zo_ ]
            , incomplete = [ Di, Zo_, Ni_ ]
            }
        , nonCadentialFoci = [ Pa_ ]
        }
    , isonOptions = [ Di, Pa ]
    , range = { start = Zo, end = Pa_ }
    , recitingTone = Zo_
    , possibleInflections =
        [ { degree = Ga, accidentals = [ Sharp2, Sharp4, Sharp6 ] }
        , { degree = Ke, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Flat4, Sharp2, Sharp4 ] }
        , { degree = Ni_, accidentals = [ Sharp4, Sharp6 ] }
        ]
    }


{-| The "Fourth-First" hybrid mode (Παραμέσος). Combines characteristics of
sticheraric lower first mode (Ananes), eirmologic medial fourth (Legetos), and
authentic fourth (Agia). All three sets of melodic attractions apply. Final
endings are on Bou; reciting tone is Pa.

  - TODO: `possibleInflections` were inferred from the constituent modes, since
    the wiki defers with "See the other modes. They basically all apply." Verify
    against repertoire examples.
  - TODO: `base = Bou` reflects that Bou is the final, but Pa drives the
    reciting character. Whether the "base" should be Bou (final) or Pa
    (functional tonal center) may need revisiting when the model is extended.

-}
authenticFour_Pa_Sticheraric : ModeData
authenticFour_Pa_Sticheraric =
    { classification = Authentic ModeFour
    , genres = [ Sticheraric ]
    , scale = Diatonic
    , dominantTones =
        { base = Bou
        , cadencePoints =
            { final = Bou
            , complete = [ Bou, Di, Pa ]
            , medial = [ Zo_, Pa, Ga ]
            , incomplete = [ Pa, Bou, Ga, Di, Zo_, Ni_ ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, Bou, Di ]
    , range = { start = DI, end = Ni_ }
    , recitingTone = Pa
    , possibleInflections =
        [ { degree = Pa, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Ga, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Ke, accidentals = [ Flat2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Flat4, Sharp2, Sharp4 ] }
        ]
    }


{-| papadic will be basically the same but for some cadence points

There are two variants: fast sticheraric will have final of Ke.

  - TODO: verify this is the right accidental for Zo

-}
plagalOne_Pa_Sticheraric : ModeData
plagalOne_Pa_Sticheraric =
    { classification = Plagal ModeOne
    , genres = [ Sticheraric ]
    , scale = Diatonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Di
            , complete = [ Di ]
            , medial = []
            , incomplete = [ Ke ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, Ke, Di, KE ]
    , range =
        { start = KE
        , end = Pa_
        }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Ga, accidentals = [ Sharp2, Sharp4, Sharp6 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| papadic will be basically the same but for some cadence points

There are two variants: fast sticheraric will have final of Ke.

  - TODO: verify this is the right accidental for Zo
  - TODO: additional details for papadic

-}
plagalOne_Pa_Papadic : ModeData
plagalOne_Pa_Papadic =
    { classification = Plagal ModeOne
    , genres = [ Papadic ]
    , scale = Diatonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = [ Di ]
            , medial = []
            , incomplete = [ Ga, Ke ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, Ke, Di, KE ]
    , range =
        { start = KE
        , end = Pa_
        }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Bou, accidentals = [ Sharp2 ] }
        , { degree = Ga, accidentals = [ Sharp2, Sharp4, Sharp6 ] }
        , { degree = Di, accidentals = [ Flat4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


plagalOne_Ke_Eirmologic : ModeData
plagalOne_Ke_Eirmologic =
    { classification = Plagal ModeOne
    , genres = [ Eirmologic ]
    , scale = Diatonic
    , dominantTones =
        { base = Ke
        , cadencePoints =
            { final = Ke
            , complete = [ Ke ]
            , medial = []
            , incomplete = [ Ni_ ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Ke ]
    , range =
        { start = Pa
        , end = Pa_
        }
    , recitingTone = Ke
    , possibleInflections =
        [ { degree = Zo_, accidentals = [ Sharp2 ] }
        , { degree = Pa_, accidentals = [ Flat2, Flat4 ] }
        , { degree = Ni_, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Ga_, accidentals = [ Flat4 ] }
        ]
    }


{-| Hard-diatonic (Enharmonic scale) variant of Aneanes plagal first, treating
upper Zo-flat as a prominent dominant tone. Shares the same sonority as the
Heptaphone variant of the grave mode on Zo-flat but does not extend into the
lower range. Ison remains on Pa.

TODO: additional cadence point detail not specified in JMB.

-}
plagalOne_Pa_Pentaphone : ModeData
plagalOne_Pa_Pentaphone =
    { classification = Plagal ModeOne
    , genres = [ Sticheraric, Papadic ]
    , scale = Enharmonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = []
            , medial = []
            , incomplete = []
            }
        , nonCadentialFoci = [ Zo_ ]
        }
    , isonOptions = [ Pa ]
    , range = { start = KE, end = Pa_ }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| Hard-diatonic Phrygian variant of Aneanes plagal first. Uses a disjoint
6–12–12 tetrachord (phthora of Ke on Pa, ajém on Bou). Phrases may cadence in
the manner of several hard-diatonic modes: Grave on Zo-flat, Plagal First
Phrygian on Pa, Third/Grave on Bou-flat or Ga, hard-diatonic Agia on Di, or
Grave on upper Zo-flat. Ison preferably remains on Pa but may follow cadences.

May also go by the name Πεντάφωνος; mind the modal signature details to
distinguish from the Pentaphone variant above.

  - TODO: Cadence structure and `possibleInflections` were inferred from the
    tetrachordal description. JMB does not provide a standard dominant-tones or
    attractions breakdown for this variant; verify against repertoire examples.

-}
plagalOne_Pa_Phrygian : ModeData
plagalOne_Pa_Phrygian =
    { classification = Plagal ModeOne
    , genres = [ Sticheraric, Papadic ]
    , scale = Enharmonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = [ Pa ]
            , medial = []
            , incomplete = [ Pa, Ga, Di ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa ]
    , range = { start = KE, end = Pa_ }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Ke, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        , { degree = Ga, accidentals = [ Sharp4, Sharp6 ] }
        ]
    }


{-| Hard-diatonic Minor (Μινόρε) variant of Aneanes plagal first. A
12–6–12 tetrachordal structure with a characteristic 6-unit upward-attracted
leading tone — essentially a Byzantine equivalent of the western classical
minor mode. Transposition of the hard-diatonic Agia onto Pa.

  - TODO: `recitingTone = Di` is assumed by analogy with other plagal-first
    modes; not specified in JMB.
  - TODO: `{ degree = Di, accidentals = [ Sharp2 ] }` is inferred from the note
    that "Ke occasionally attracts Di upwards"; the specific moria value is not
    stated in the wiki.

-}
plagalOne_Pa_Minor : ModeData
plagalOne_Pa_Minor =
    { classification = Plagal ModeOne
    , genres = [ Sticheraric, Papadic ]
    , scale = Enharmonic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Pa
            , complete = [ Pa ]
            , medial = []
            , incomplete = [ Pa, Ga, Ke ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, Ga, Ke ]
    , range = { start = KE, end = Pa_ }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Ni, accidentals = [ Sharp6 ] }
        , { degree = Di, accidentals = [ Sharp2 ] }
        ]
    }


{-| Hard chromatic plagal second on Pa. Unusually, the base (Pa) is not the
final: Pa serves all cadence types except final endings, which resolve to Di.
The soft-chromatic phthora may appear on the Pa–Bou–Ga trichord or Pa–Ni–Zo
for expressive colour.

  - TODO: `{ degree = Ga, accidentals = [ Flat2 ] }` is uncertain. The wiki
    says the Bou–Ga interval "may sometimes be 18, not 20" moria, implying Ga
    is lowered (hence `Flat2`). But the wiki also says this degree "may sometimes
    be attracted upward even further at some cadence points", which appears
    contradictory. The direction and mechanism of this attraction need expert
    clarification.

-}
plagalTwo_Pa : ModeData
plagalTwo_Pa =
    { classification = Plagal ModeTwo
    , genres = [ Sticheraric, Papadic ]
    , scale = HardChromatic
    , dominantTones =
        { base = Pa
        , cadencePoints =
            { final = Di
            , complete = [ Pa ]
            , medial = [ Pa ]
            , incomplete = [ Pa, Di, Ke, Ga ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Pa, Ke, Di ]
    , range = { start = DI, end = Pa_ }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Ni, accidentals = [ Sharp2, Sharp4, Sharp6 ] }
        , { degree = Ga, accidentals = [ Flat2 ] }
        ]
    }


{-| Standard grave mode, eirmologic and sticheraric. The basic scale is soft
diatonic, but the persistent ajém on Zo creates a consistently hard-diatonic
character in practice. Ni is the most prominent ison choice, moreso than Ga
itself. Begins on Ga, moves to Di for intoning, then cadences per the style.

  - TODO: `scale = Diatonic` reflects the theoretical soft-diatonic foundation.
    The practical sound is hard-diatonic due to ajém; `Enharmonic` may be more
    accurate. This parallels the same ambiguity in `authenticThree_Ga_Eirmologic`
    and the two fields should be resolved consistently.

-}
plagalThree_Ga : ModeData
plagalThree_Ga =
    { classification = Plagal ModeThree
    , genres = [ Eirmologic, Sticheraric ]
    , scale = Diatonic
    , dominantTones =
        { base = Ga
        , cadencePoints =
            { final = Ga
            , complete = [ Ga ]
            , medial = []
            , incomplete = [ Di, Pa ]
            }
        , nonCadentialFoci = [ Ni, Zo_ ]
        }
    , isonOptions = [ Ni, Ga, Di ]
    , range = { start = Ni, end = Ga_ }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Bou, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Pa, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Di, accidentals = [ Sharp2, Sharp4 ] }
        ]
    }


{-| Soft-diatonic grave mode on Zo (Πρωτόβαρυς, "First-Grave"). A neighbor
mode that spends much of its time evoking other modes. The three submodes
indicate which lower-first variant is primarily at work:

  - Τετράφωνος (at the fourth, Ga reciting tone)
  - Πεντάφονος (at the fifth, Di reciting tone)
  - Ἑπτάφωνος (at the octave, upper Zo reciting tone)

Ke is sharpened consistently when approaching Zo (base), much as Pa is
sharpened in Legetos.

  - TODO: `recitingTone = Ga` represents only the Tetraphonos submode. The
    Pentaphonos submode uses Di and the Eptaphonos uses upper Zo. The current
    `ModeData` model has a single `recitingTone` field and cannot represent all
    three variants simultaneously.

-}
plagalThree_Zo : ModeData
plagalThree_Zo =
    { classification = Plagal ModeThree
    , genres = [ Sticheraric, Papadic ]
    , scale = Diatonic
    , dominantTones =
        { base = Zo_
        , cadencePoints =
            { final = Zo_
            , complete = [ Zo_, Pa ]
            , medial = []
            , incomplete = [ Pa ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Zo_, Pa ]
    , range = { start = Di, end = Pa_ }
    , recitingTone = Ga
    , possibleInflections =
        [ { degree = Ke, accidentals = [ Sharp4, Sharp6 ] }
        , { degree = Ga, accidentals = [ Sharp2 ] }
        ]
    }


{-| Hard-diatonic grave mode on Zo-flat. Almost exclusively papadic and fully
written out, so the reciting tone is not applicable in the usual sense. Zo-flat
is expressed via the Enharmonic scale; the base degree is Zo\_.

  - TODO: `recitingTone = Zo_` is a structural placeholder only; JMB explicitly
    states the concept is not applicable for this mode.
  - TODO: Verify accidentals for upper zo-flat cadence (Heptaphone variant).

-}
plagalThree_Zo_Hard : ModeData
plagalThree_Zo_Hard =
    { classification = Plagal ModeThree
    , genres = [ Sticheraric, Papadic ]
    , scale = Enharmonic
    , dominantTones =
        { base = Zo_
        , cadencePoints =
            { final = Zo_
            , complete = [ Zo_ ]
            , medial = []
            , incomplete = [ Zo_, Pa, Ga, Di ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Zo_, Ga, Di ]
    , range = { start = DI, end = Ni_ }
    , recitingTone = Zo_
    , possibleInflections =
        [ { degree = Ke, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Di, accidentals = [ Sharp4 ] }
        , { degree = Ga, accidentals = [ Sharp4, Sharp6 ] }
        ]
    }


{-| Standard plagal fourth mode (Neagie). Begins and ends on Ni, moves quickly
to Bou as a medial reciting tone, then up to Di as the main reciting tone. The
upper Agia tetrachord (Di–Ni) comes into play when the melody ascends, shifting
the ison to Di or lower Di in anticipation of final cadences.
-}
plagalFour_Ni : ModeData
plagalFour_Ni =
    { classification = Plagal ModeFour
    , genres = [ Eirmologic, Sticheraric, Papadic ]
    , scale = Diatonic
    , dominantTones =
        { base = Ni
        , cadencePoints =
            { final = Ni
            , complete = [ Di ]
            , medial = [ Bou ]
            , incomplete = [ Di ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Ni, Di, DI ]
    , range = { start = GA, end = Ni_ }
    , recitingTone = Di
    , possibleInflections =
        [ { degree = Zo, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Pa, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Ga, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Zo_, accidentals = [ Flat4 ] }
        ]
    }


{-| Triphone Neagie / Nana on Ga (Εirmologic). Variant of plagal fourth where
Ga becomes the effective base, using the phthora of Ni on Ga to transpose Ni's
soft-diatonic tetrachord up a fourth. Ga functions analogously to Ni in the
standard Neagie; Ke parallels the role of Bou.

  - TODO: The wiki describes two variants: a first (hard-diatonic, melodically
    indistinguishable from Third Mode or Grave Mode) and a second (soft-diatonic
    with phthora of Ni on Ga, used here). Only the second variant is encoded;
    the first variant is unrepresented and may warrant its own `Mode` entry.

-}
plagalFour_Ga : ModeData
plagalFour_Ga =
    { classification = Plagal ModeFour
    , genres = [ Eirmologic ]
    , scale = Diatonic
    , dominantTones =
        { base = Ga
        , cadencePoints =
            { final = Ga
            , complete = [ Ga ]
            , medial = []
            , incomplete = [ Di, Ke, Ni ]
            }
        , nonCadentialFoci = []
        }
    , isonOptions = [ Ga, Ni, Di ]
    , range = { start = Ni, end = Ga_ }
    , recitingTone = Ga
    , possibleInflections =
        [ { degree = Bou, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Di, accidentals = [ Sharp2, Sharp4 ] }
        , { degree = Pa, accidentals = [ Sharp2, Sharp4 ] }
        ]
    }
