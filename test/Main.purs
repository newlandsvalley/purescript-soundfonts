module Test.Main where

import Prelude

import Audio.SoundFont.Gleitz (midiPitch)
import Audio.SoundFont.Melody (Melody)
import Audio.SoundFont.Melody.Class (class Playable, MidiRecording(..), toMelody)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free)
import Data.List (List(..), (:), singleton)
import Data.Array (concat, length)
import Data.Map (empty)
import Data.Midi as Midi
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


gleitzSuite :: forall t. Free (TestF t) Unit
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

playableSuite :: forall t. Free (TestF t) Unit
playableSuite =
  suite "playable" do
    test "melody generation from midi" do
      Assert.equal 2 (length generateMelody)          -- 2 phrases
      Assert.equal 4 (length $ concat generateMelody) -- 4 notes overall

main :: forall t.
        Eff
          ( console :: CONSOLE
          , testOutput :: TESTOUTPUT
          , avar :: AVAR
          | t
          )
          Unit
main = runTest do
  suite "soundfonts" do
    gleitzSuite
    playableSuite

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
