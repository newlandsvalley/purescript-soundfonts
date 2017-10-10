module Main where

import Prelude (bind, ($), (*))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Fiber, launchAff, delay)
import Data.Array (singleton)
import Network.HTTP.Affjax (AJAX)
import Data.Time.Duration (Milliseconds(..))
import Audio.SoundFont (AUDIO, MidiNote, logLoadResource, loadInstrument, loadInstruments, playNote, playNotes)

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

{- just for debug
main :: ∀ e.
  Eff (ajax :: AJAX, console :: CONSOLE | e) (Fiber (ajax :: AJAX, console :: CONSOLE | e) Unit)
main = do
  _ <- logLoadResource "acoustic_grand_piano"
  logLoadResource "marimba"
-}



main :: ∀ eff.
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
main = launchAff $ do
  -- instrument <- loadInstrument "acoustic_grand_piano"
  instruments <- loadInstruments ["marimba", "acoustic_grand_piano", "tango_accordion"]

  da <- liftEff $ playNote instruments noteSampleA
  _ <- delay (Milliseconds $ 1000.0 * da)
  db <- liftEff $ playNote instruments noteSampleC
  _ <- delay (Milliseconds $ 1000.0 * db)
  de <- liftEff $ playNote instruments noteSampleE
  _ <- delay (Milliseconds $ 1000.0 * de)
  liftEff $ playNotes instruments (notesSample 2)

{-}
main :: ∀ eff.
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
main = launchAff $ do
  -- instrument <- loadInstrument "acoustic_grand_piano"
  instrument <- loadInstrument "marimba"
  let
    instruments = singleton instrument
  da <- liftEff $ playNote instruments noteSampleA
  _ <- delay (Milliseconds $ 1000.0 * da)
  db <- liftEff $ playNote instruments noteSampleC
  _ <- delay (Milliseconds $ 1000.0 * db)
  de <- liftEff $ playNote instruments noteSampleE
  _ <- delay (Milliseconds $ 1000.0 * de)
  liftEff $ playNotes instruments (notesSample 0)
  -- pure font
-}
