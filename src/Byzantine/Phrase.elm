module Byzantine.Phrase exposing (..)

{-| Experimental type for modelling a phrase of Byzantine chant. First use is
intended to be modal apichima. The end goal is to support complete melodic
composition so that any typesafe Phrase is guaranteed to be compositionally
logical with respect to orthography.


# Phrase

@docs Phrase


# Syllable

@docs Syllable


# Word

@docs Word

-}

import Byzantine.IntervalCharacter exposing (IntervalCharacter)


{-| Model Phrase as just a basic List (i.e., let `[]` be a legitimate option)
-}
type Phrase
    = Phrase (List Word)


{-| A word must contain at least one syllable.

There will be rendering implications for relationships of syllables.

  - English will use hyphens to connect syllables within a word

  - English will use an underscore to extend an ultimate syllable

  - Greek will repeat the vowel for each neume group to extend a syllable

I'm not sure if this is something that should be modeled, but am leaning towards
not. It might be better to leave this as a rendering concern rather than a data
modeling concern.

-}
type Word
    = Word
        { initial : Syllable
        , remaining : List Syllable
        }


{-| A syllable must contain at least one beat group.

What we'll probably want is some sort of `Syllable -> List Neume` function used
for rendering, where `Neume` is the presentational layer, and `Syllable` is the
theoretical musical layer. But the Syllable should contain sufficient
information so that the presentational layer is determenistic.

Here's an idea to unpack: what would be the implications of having different
types for the initial and continuation beat groups? There are some characters or
character combinations that cannot support a syllable on their own, plus some
possible wrinkles with gorgons over kentimata where the kentimata are below the
main interval character. A type-level distinction between initial syllablic
beats and continuation beats may be highly useful in modeling these constraints.
We'll definitely need better names with all these, though.

-}
type Syllable
    = Syllable
        { initial : BeatGroup
        , remaining : List BeatGroup
        , text : String
        }


{-| "Beat" may be misleading, given that we're folding "extension" into the
concept.

`division` and `extension` are not mutually exclusive.

Running elafron is going to be sticky as it comes to syllable structure. I think
we may need to separate out the notional convention from musical. Musically, it
functions by dividing beat of the previous syllable; notationally, it is
attached to the following syllable. We'll need to double-check this with Boyer
to make sure that things make sense.

-}
type BeatGroup
    = BeatGroup
        { intervalCharacter : IntervalCharacter
        , division : Maybe Division
        , extension : Maybe Extension
        , baria : Maybe Baria
        , ornamentation : Maybe Ornamentation
        }


{-| Gorgon is prototypical, but running elafron should be included in this. The
other tricky thing is going to be kentimata placement and how does this interact
with notational concepts.

There's a chance this may need to be elevated up a level so it's applied at the
syllable layer where appropriate domain constarints can be modeled.

-}
type Division
    = IntoHalves
    | IntoThirds
    | IntoQuarters


{-| There may be better terms fo this, but I'd like to keep a degree of
decoupling between the theoretical musical modeling and the presentation layer
(notation).

Check to see if the klasma vs dot can be considered a presentational issue,
i.e., is this determined by type of character that is being extended, or
context, or something else, so that this doesn't need to be encoded at this
level?

-}
type Extension
    = OneBeat_Klasma
    | OneBeat_Dot
    | TwoBeats
    | ThreeBeats


{-| The accent character. Related to ornaments, but notationally comes before
the element rather than under it. Also, a character can have both the baria and
some other expressive character of quality. Also, we'll need to check to see if
it's appropriate for this to be attached at the syllable level, or is this
something that is only approriate at the start of a word.
-}
type Baria
    = Baria


{-| Ornamentation here are the characters of quality.
-}
type Ornamentation
    = TODO_Ornamentation


{-| We'll want to incorporate a metrical position aspect into some layer,
possibly both syllable _and_ beat group. I'm not quite sure what the
implications and/or constraints should be yet, though.
-}
type MetricalPosition
    = Accented
    | Unaccented
