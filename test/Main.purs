module Test.Main where

-- | Because SoundFonts run in the browser, not in Node, we're a little restricted
-- | in what we can test. In particular, we can check that we can read a Gleitzman
-- | SoundFont file and decode it to a Map of array buffers, but not that we can
-- | convert it to an Instrument (of audioBuffers) with web-audio.

import Prelude

import Audio.SoundFont.Gleitz (midiPitch)
import Audio.SoundFont.Decoder (NoteMap, midiJsToNoteMap)
import Audio.SoundFont.Melody (Melody)
import Audio.SoundFont.Melody.Class (MidiRecording(..), toMelody)
import Control.Monad.Free (Free)
import Data.List (List(..), (:), singleton)
import Data.Array (concat, length)
import Data.Either (either)
import Data.Map (empty)
import Data.Map.Internal (size)
import Data.Midi as Midi
import Test.Unit (Test, TestF, suite, test, failure, success)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Effect (Effect)
import Node.FS.Aff (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Midi.Instrument (InstrumentName(AcousticGrandPiano))

gleitzSuite :: Free TestF Unit
gleitzSuite =
  suite "gleitz" do
    test "midi pitch C4" do
      Assert.equal 60 (midiPitch "C4")
    test "midi pitch Bb1" do
      Assert.equal 34 (midiPitch "Bb1")
    test "midi pitch A#1" do
      Assert.equal 34 (midiPitch "A#1")
    test "midi pitch A4" do
      Assert.equal 69 (midiPitch "A4")
    test "midi pitch C8" do
      Assert.equal 108 (midiPitch "C8")

playableSuite :: Free TestF Unit
playableSuite =
  suite "playable" do
    test "melody generation from midi" do
      Assert.equal 2 (length generateMelody)          -- 2 phrases
      Assert.equal 4 (length $ concat generateMelody) -- 4 notes overall

decodeSuite :: Free TestF Unit
decodeSuite =
  suite "decode" do
    test "decode json to array buffer Map" do
      pianoFontJson <- readTextFile UTF8 "gleitz/acoustic_grand_piano-ogg.js"
      let
        eNoteMap = midiJsToNoteMap AcousticGrandPiano pianoFontJson
      either (\e -> failure $ show e) checkNoteMapSize eNoteMap

checkNoteMapSize :: NoteMap -> Test
checkNoteMapSize noteMap =
  Assert.equal 88 (size noteMap)

main :: Effect Unit
main = runTest do
  suite "soundfonts" do
    gleitzSuite
    playableSuite
    decodeSuite

generateMelody :: Melody
generateMelody =
  toMelody (MidiRecording recording) empty

-- small MIDI example
recording :: Midi.Recording
recording =
  Midi.Recording
    { header : header
    , tracks : singleton track0
    }
  where

    header :: Midi.Header
    header =
      Midi.Header
        { formatType : 0
        , trackCount : 1
        , ticksPerBeat : 240
        }

    track0 :: Midi.Track
    track0 =
      Midi.Track $ (note 62) <> (note 64) <> (note 65) <> (note 67)

note :: Int -> List Midi.Message
note pitch =
    (Midi.Message 0 $ Midi.NoteOn 0 pitch 100)
  : (Midi.Message 60 $ Midi.NoteOff 0 pitch 100)
  : Nil
