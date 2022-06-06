module Test.Main where

-- | Because SoundFonts run in the browser, not in Node, we're a little restricted
-- | in what we can test. In particular, we can check that we can read a Gleitzman
-- | SoundFont file and decode it to a Map of array buffers, but not that we can
-- | convert it to an Instrument (of audioBuffers) with web-audio.

import Prelude

import Audio.SoundFont.Decoder (NoteMap, midiJsToNoteMap)
import Audio.SoundFont.Gleitz (midiPitch)
import Audio.SoundFont.Melody (Melody)
import Audio.SoundFont.Melody.Class (MidiRecording(..), toMelody)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (concat, length)
import Data.Either (either)
import Data.List (List(..), (:), singleton)
import Data.Map (empty)
import Data.Map.Internal (size)
import Data.Midi as Midi
import Data.Midi.Instrument (InstrumentName(AcousticGrandPiano))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

gleitzSpec :: Spec Unit
gleitzSpec =
  describe "gleitz" do
    it "knows midi pitch C4" do
      60 `shouldEqual` (midiPitch "C4")
    it "knows midi pitch Bb1" do
      34 `shouldEqual` (midiPitch "Bb1")
    it "knows midi pitch A#1" do
      34 `shouldEqual` (midiPitch "A#1")
    it "knows midi pitch A4" do
      69 `shouldEqual` (midiPitch "A4")
    it "knows midi pitch C8" do
      108 `shouldEqual` (midiPitch "C8")

playableSpec :: Spec Unit
playableSpec =
  describe "playable" do
    it "generate melody from midi" do
      2 `shouldEqual` (length generateMelody) -- 2 phrases
      4 `shouldEqual` (length $ concat generateMelody) -- 4 notes overall

decodeSpec :: Spec Unit
decodeSpec =
  describe "decode" do
    it "decodes json to array buffer Map" do
      pianoFontJson <- readTextFile UTF8 "gleitz/acoustic_grand_piano-ogg.js"
      let
        eNoteMap = midiJsToNoteMap AcousticGrandPiano pianoFontJson
      either (\e -> fail $ show e) checkNoteMapSize eNoteMap

checkNoteMapSize :: forall m. MonadThrow Error m => NoteMap -> m Unit
checkNoteMapSize noteMap =
  88 `shouldEqual` (size noteMap)

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  describe "soundfonts" do
    gleitzSpec
    playableSpec
    decodeSpec

generateMelody :: Melody
generateMelody =
  toMelody (MidiRecording recording) empty

-- small MIDI example
recording :: Midi.Recording
recording =
  Midi.Recording
    { header: header
    , tracks: singleton track0
    }
  where

  header :: Midi.Header
  header =
    Midi.Header
      { formatType: 0
      , trackCount: 1
      , ticksPerBeat: 240
      }

  track0 :: Midi.Track
  track0 =
    Midi.Track $ (note 62) <> (note 64) <> (note 65) <> (note 67)

note :: Int -> List Midi.Message
note pitch =
  (Midi.Message 0 $ Midi.NoteOn 0 pitch 100)
    : (Midi.Message 60 $ Midi.NoteOff 0 pitch 100)
    : Nil
