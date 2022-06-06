module Audio.SoundFont.Melody
  ( MidiPhrase
  , Melody
  , PMelody(..)
  , playPhrase
  , playMelody
  ) where

import Audio.SoundFont (Instrument, MidiNote, playNotes)
import Data.Foldable (traverse_)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Prelude

-- | A Melody is simply an array of phrases where each phrase is an 
-- | Array of notes that can be played using playNotes.
-- | It is also the entity that is played by the Halogen player widget.
type Melody = Array MidiPhrase

-- | We split up a melody into phrases to allow the player to be
-- | re-rendered after each phrase is played
type MidiPhrase = Array MidiNote

newtype PMelody = PMelody Melody

-- | Play a phrase of MidiNotes.
-- | This is identical to playNotes except that it invokes a delay 
-- | equal to the duration of the phrase.  This allows multiple 
-- | phrased to be paced properly.
playPhrase :: Array Instrument -> MidiPhrase -> Aff Unit
playPhrase instruments phrase = do
  duration <- liftEffect $ playNotes instruments phrase
  delay (Milliseconds $ 1000.0 * duration)

-- | Play the entire Melody
playMelody :: Array Instrument -> Melody -> Aff Unit
playMelody instruments melody =
  traverse_ (playPhrase instruments) melody
