module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)

import Test.Unit (TestF, suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert as Assert
import Audio.SoundFont.Gleitz (midiPitch)

gleitzSuite :: forall t. Free (TestF t) Unit
gleitzSuite =
  suite "gleitz" do
    test "midi pitch C4" do
      Assert.equal 36 (midiPitch "C4")
    test "midi pitch Bb1" do
      Assert.equal 10 (midiPitch "Bb1")
    test "midi pitch A#1" do
      Assert.equal 10 (midiPitch "A#1")
    test "midi pitch A4" do
      Assert.equal 45 (midiPitch "A4")
    test "midi pitch C8" do
      Assert.equal 84 (midiPitch "C8")

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
