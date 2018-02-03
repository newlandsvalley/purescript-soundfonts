module Audio.SoundFont.Melody.Class where

-- | a typeclass representing those sound sources that can be played
-- | by a soundfont player

import Audio.SoundFont.Melody (Melody, PMelody(..))
import Audio.SoundFont.Melody.Maker (toMelody) as MM
import Data.Midi as Midi

class Playable p where
  toMelody :: p -> Melody

instance playableMelody :: Playable PMelody where
  toMelody (PMelody melody) = melody

newtype MidiRecording = MidiRecording Midi.Recording

instance playableMidi :: Playable MidiRecording where
  toMelody (MidiRecording recording) = MM.toMelody recording
