module Example.Main where

import Prelude (Unit, bind, discard, map, pure, unit, ($), (*), (>>=))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Fiber, launchAff, delay)
import Effect.Exception (throw)
import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Audio.SoundFont (MidiNote
  , loadRemoteSoundFonts
  , playNote
  , playNotes)
import Data.Midi.Instrument (InstrumentName(..))
import Web.DOM.ParentNode (querySelector)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)
import Unsafe.Coerce (unsafeCoerce)

note :: Int -> Int -> Number -> Number -> Number -> MidiNote
note channel id timeOffset duration gain =
  { channel : channel, id : id, timeOffset : timeOffset, duration : duration, gain : gain }

noteSampleA :: MidiNote
noteSampleA = note 0 57 0.0 0.5 1.0

noteSampleC :: MidiNote
noteSampleC = note 0 60 0.0 0.5 1.0

noteSampleE :: MidiNote
noteSampleE = note 0 64 0.0 0.5 1.0

notesSample :: Int -> Array MidiNote
notesSample channel =
 [ note channel 60 1.0 0.5 1.0
 , note channel 62 1.5 0.5 1.0
 , note channel 64 2.0 0.5 1.0
 , note channel 65 2.5 0.5 1.0
 , note channel 67 3.0 1.5 1.0
 , note channel 71 3.0 1.5 1.0
 ]

-- | load remote fonts example

main :: Effect Unit
main = do
  -- a user gesture is required before the browser is allowed to use web-audio
  doc <- map toParentNode (window >>= document)
  play <- querySelector (wrap "#play") doc
  case play of
    Just e -> do
      el <- eventListener \_ -> playExample
      addEventListener (wrap "click") el false (unsafeCoerce e :: EventTarget)
    Nothing -> throw "No 'play' button"
  pure unit

playExample :: Effect (Fiber Number)
playExample = launchAff $ do
  instruments <- loadRemoteSoundFonts [Marimba, AcousticGrandPiano, TangoAccordion]

  _ <- delay (Milliseconds $ 1000.0)
  da <- liftEffect $ playNote instruments noteSampleA
  _ <- delay (Milliseconds $ 1000.0 * da)
  db <- liftEffect $ playNote instruments noteSampleC
  _ <- delay (Milliseconds $ 1000.0 * db)
  de <- liftEffect $ playNote instruments noteSampleE
  _ <- delay (Milliseconds $ 1000.0 * de)
  liftEffect $ playNotes instruments (notesSample 2)



{-
-- | load local piano font example
pianoExample :: ∀ eff.
  Eff
    ( ajax :: AJAX
    , au :: AUDIO
    | eff
    )
    (Fiber
       ( ajax :: AJAX
       , au :: AUDIO
       | eff
       )
       Number
    )
pianoExample = launchAff $ do
  -- instrument <- loadInstrument (Just "soundfonts") "acoustic_grand_piano"
  instrument <- loadPianoSoundFont "soundfonts"
  let
    instruments = singleton instrument
  da <- liftEff $ playNote instruments noteSampleA
  _ <- delay (Milliseconds $ 1000.0 * da)
  db <- liftEff $ playNote instruments noteSampleC
  _ <- delay (Milliseconds $ 1000.0 * db)
  de <- liftEff $ playNote instruments noteSampleE
  _ <- delay (Milliseconds $ 1000.0 * de)
  liftEff $ playNotes instruments (notesSample 0)
-}
