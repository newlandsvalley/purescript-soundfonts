module Audio.SoundFont.Gleitz
  ( RecordingFormat(..)
  , SoundFontType(..)
  , gleitzUrl
  , gleitzNoteName
  , midiPitch
  , debugNoteName
  ) where

import Prelude (class Show, (<>), ($), (+), (*), map, negate, show)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.Array.NonEmpty (NonEmptyArray, index)
import Data.Tuple (Tuple(..))
import Data.Map (Map, fromFoldable, lookup)
import Data.Midi.Instrument (InstrumentName, gleitzmanName)
import Partial.Unsafe (unsafePartial)

-- | Provide descriptions of MIDI.js from https://github.com/gleitz/midi-js-soundfonts
-- | which describes each note in terms of its pitch class.
-- | Provide a translation of note names to MIDI note ids.

data RecordingFormat = MP3 | OGG

instance showRecordingFormat :: Show RecordingFormat where
  show MP3 = "mp3"
  show OGG = "ogg"

data SoundFontType = Fluid3 | MusyngKite | FatBoy

instance showSoundFontType :: Show SoundFontType where
  show Fluid3 = "FluidR3_GM"
  show MusyngKite = "MusyngKite"
  show FatBoy = "FatBoy"

type Pitch = String

-- | note sequences start at C with the normal intervals between the white notes
semitones :: Map String Int
semitones = fromFoldable
  [ Tuple "C" 0
  , Tuple "D" 2
  , Tuple "E" 4
  , Tuple "F" 5
  , Tuple "G" 7
  , Tuple "A" 9
  , Tuple "B" 11
  ]

gleitzBaseUrl :: String
gleitzBaseUrl = "https://gleitz.github.io/midi-js-soundfonts/"

gleitzUrl :: InstrumentName -> SoundFontType -> RecordingFormat -> String
gleitzUrl instrument fontType format =
  gleitzBaseUrl <> (show fontType) <> "/" <> (gleitzmanName instrument) <> "-" <> (show format) <> ".js"

-- at the moment - just throw away the error
midiPitch :: String -> Int
midiPitch s =
  fromMaybe 0 (midiPitch1 s)

-- | convert a Gleitz note name to a MIDI pitch
midiPitch1 :: String -> Maybe Int
midiPitch1 s =
  case gleitzNoteName s of
    Nothing -> Nothing
    Just matches ->
      let
        mpitch :: Maybe Int
        mpitch = case index matches 1 of -- first match group
          Just (Just p) -> lookup p semitones -- A-G
          _ -> Nothing

        acc :: Int
        acc = case index matches 2 of -- second match group
          Just (Just "b") -> (-1) -- # or b
          Just (Just "#") -> 1 -- # or b
          _ -> 0

        moctave :: Maybe Int
        moctave = -- third match group

          case index matches 3 of -- octave number
            Just (Just octave) -> fromString octave
            _ -> Nothing
      in
        case (Tuple mpitch moctave) of
          (Tuple (Just p) (Just oct)) -> Just $ buildMidiPitch oct p acc
          _ -> Nothing

-- | build a MIDI pitch (middle C = C4)
-- | building from the octave number, pitch (relative to C)
-- | and any accidental offset (i.e. flat lowers pitch by 1, sharp raised it)
-- | The MIDI standard does not standardise on a particular middle C
buildMidiPitch :: Int -> Int -> Int -> Int
buildMidiPitch octave pitch accidental =
  (12 * octave) + pitch + accidental + 12

{-
lookupPitch :: Pitch -> Maybe Int
lookupPitch p =
  lookup (toUpper p) semitones
-}

-- | this should parse Bb0 into
-- | Just [(Just "Bb0"),(Just "B"),(Just "b"),(Just "0")]
-- | and C8 into
-- | Just [(Just "C8"),(Just "C"),(Just ""),(Just "8")]
gleitzNoteName :: String -> Maybe (NonEmptyArray (Maybe String))
gleitzNoteName s =
  let
    makeRegex :: Partial => Regex.Regex
    makeRegex =
      case Regex.regex "([A-Ga-g])([b#]?)([0-8])" noFlags of
        Right r ->
          r
    regex = unsafePartial makeRegex
  in
    Regex.match regex s

--| debug
debugNoteName :: String -> String
debugNoteName s =
  "( " <> s <> ": " <> (fromMaybe "error" $ map show $ midiPitch1 s) <> ")"
