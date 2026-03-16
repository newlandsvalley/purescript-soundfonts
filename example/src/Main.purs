module Main where

import Prelude (Unit, bind, discard, map, pure, unit, ($), (*), (>>=))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, Fiber, launchAff, delay)
import Effect.Exception (throw)
import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Unfoldable (replicate)
import Audio.SoundFont (Instrument
  , MidiNote
  , loadRemoteSoundFonts
  , midiNote
  , playNote
  , playNotes)
import Audio.SoundFont.Melody (playMelody)
import Data.Midi (Channel(..), MidiPitch(..))
import Data.Midi.Instrument (InstrumentName(..))
import Web.DOM.ParentNode (querySelector)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)
import Unsafe.Coerce (unsafeCoerce)

noteSampleA :: MidiNote
noteSampleA = midiNote (Channel 0) (MidiPitch 57) 0.0 0.5 1.0

noteSampleC :: MidiNote
noteSampleC = midiNote (Channel 0) (MidiPitch 60) 0.0 0.5 1.0

noteSampleE :: MidiNote
noteSampleE = midiNote (Channel 0) (MidiPitch 64) 0.0 0.5 1.0

notesSample :: Channel -> Array MidiNote
notesSample channel =
 [ midiNote channel (MidiPitch 60) 1.0 0.5 1.0
 , midiNote channel (MidiPitch 62) 1.5 0.5 1.0
 , midiNote channel (MidiPitch 64) 2.0 0.5 1.0
 , midiNote channel (MidiPitch 65) 2.5 0.5 1.0
 , midiNote channel (MidiPitch 67) 3.0 1.5 1.0
 , midiNote channel (MidiPitch 71) 3.0 1.5 1.0
 ]

main :: Effect Unit
main = do
  -- a user gesture is required before the browser is allowed to use web-audio
  doc <- map toParentNode (window >>= document)
  play <- querySelector (wrap "#play") doc
  case play of
    Just e -> do
      el <- eventListener \_ -> playAll
      addEventListener (wrap "click") el false (unsafeCoerce e :: EventTarget)
    Nothing -> throw "No 'play' button"
  pure unit

playAll :: Effect (Fiber Unit)
playAll = launchAff $ do
  instruments <- loadRemoteSoundFonts [Marimba, AcousticGrandPiano, TangoAccordion]
  _ <- playNotesExample instruments 
  playMelodyExample instruments 


-- | play example using playNotes
playNotesExample :: Array Instrument -> Aff Unit
playNotesExample instruments = do
  da <- liftEffect $ playNote instruments noteSampleA
  _ <- delay (Milliseconds $ 1000.0 * da)
  db <- liftEffect $ playNote instruments noteSampleC
  _ <- delay (Milliseconds $ 1000.0 * db)
  de <- liftEffect $ playNote instruments noteSampleE
  _ <- delay (Milliseconds $ 1000.0 * de)
  df <- liftEffect $ playNotes instruments (notesSample (Channel 2))
  delay (Milliseconds $ 1000.0 * df)

-- playMelody example (on the piano)
playMelodyExample :: Array Instrument -> Aff Unit
playMelodyExample instruments = do
  let
    melody = replicate 3 (notesSample (Channel 1))
  playMelody instruments melody

