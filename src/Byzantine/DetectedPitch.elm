module Byzantine.DetectedPitch exposing (DetectedPitch, fromFrequency)

{-| Utilities for analyzing detected pitch frequencies and converting them to
degree information.

@docs DetectedPitch, fromFrequency

-}

import Array
import Byzantine.Degree as Degree exposing (Degree)
import Byzantine.Frequency as Frequency exposing (Frequency)
import Byzantine.PitchPosition as PitchPosition exposing (PitchPosition)
import Maybe.Extra as Maybe
import Model.AudioSettings exposing (AudioSettings)
import Model.DegreeDataDict as DegreeDataDict exposing (DegreeDataDict)
import Model.PitchSpaceData exposing (PitchSpaceData)


{-| Represents a detected pitch with its frequency, closest degree, and offset
from that degree.
-}
type alias DetectedPitch =
    { frequency : Frequency
    , degree : Degree
    , offset : Float
    }


{-| Create a DetectedPitch from a Frequency using audio settings and pitch space
data.
-}
fromFrequency : AudioSettings -> PitchSpaceData -> Frequency -> DetectedPitch
fromFrequency audioSettings pitchSpaceData frequency =
    let
        detectedPitchInMoria =
            Frequency.toPitchPosition
                audioSettings.pitchStandard
                audioSettings.listenRegister
                frequency

        degreeInfo =
            closestDegree pitchSpaceData.pitchPositions detectedPitchInMoria
    in
    { frequency = frequency
    , degree = degreeInfo.degree
    , offset = degreeInfo.offset
    }


{-| Find the closest degree to a detected pitch position and calculate the
offset.
-}
closestDegree : DegreeDataDict PitchPosition -> Float -> { degree : Degree, offset : Float }
closestDegree pitchPositions detectedPitchInMoria =
    let
        initialGuessDegreeIndex =
            floor (detectedPitchInMoria / 72 * 7)

        initialGuessDegree =
            Array.get initialGuessDegreeIndex Degree.gamut
                |> Maybe.withDefaultLazy
                    (\_ ->
                        if initialGuessDegreeIndex < 0 then
                            Degree.GA

                        else
                            Degree.Ga_
                    )
    in
    closestDegreeHelper pitchPositions detectedPitchInMoria initialGuessDegree


closestDegreeHelper : DegreeDataDict PitchPosition -> Float -> Degree -> { degree : Degree, offset : Float }
closestDegreeHelper pitchPositions detectedPitch lowerNeighborCandidate =
    let
        lowerNeighborCandidatePosition =
            PitchPosition.toFloat (DegreeDataDict.get lowerNeighborCandidate pitchPositions)
    in
    case ( compare lowerNeighborCandidatePosition detectedPitch, Degree.step lowerNeighborCandidate 1 ) of
        ( GT, _ ) ->
            case Degree.step lowerNeighborCandidate -1 of
                Just newTest ->
                    closestDegreeHelper pitchPositions detectedPitch newTest

                Nothing ->
                    { degree = lowerNeighborCandidate, offset = detectedPitch - lowerNeighborCandidatePosition }

        ( EQ, _ ) ->
            -- don't hold your breath for this one.
            { degree = lowerNeighborCandidate, offset = 0 }

        ( LT, Nothing ) ->
            { degree = lowerNeighborCandidate, offset = detectedPitch - lowerNeighborCandidatePosition }

        ( LT, Just upperNeighborCandidate ) ->
            let
                upperNeighborCandidatePosition =
                    PitchPosition.toFloat (DegreeDataDict.get upperNeighborCandidate pitchPositions)
            in
            if detectedPitch < upperNeighborCandidatePosition then
                if abs (lowerNeighborCandidatePosition - detectedPitch) < abs (upperNeighborCandidatePosition - detectedPitch) then
                    { degree = lowerNeighborCandidate, offset = detectedPitch - lowerNeighborCandidatePosition }

                else
                    { degree = upperNeighborCandidate, offset = detectedPitch - upperNeighborCandidatePosition }

            else
                case Degree.step upperNeighborCandidate 1 of
                    Just newTest ->
                        closestDegreeHelper pitchPositions detectedPitch newTest

                    Nothing ->
                        { degree = upperNeighborCandidate, offset = detectedPitch - upperNeighborCandidatePosition }
