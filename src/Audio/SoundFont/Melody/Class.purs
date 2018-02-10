module Audio.SoundFont.Melody.Class where

-- | A typeclass representing those sound sources that can be played
-- | by a soundfont player.  The InstrumentChannels argument is intended
-- | for use with those sources which need to establish the MIDI channel
-- | of each note from the MIDI instruments that are in scope. If it is
-- | not required, users can supply the value empty.

-- At the time of writing, PureScript does not allow default implementations
-- within tyoeclass definitions.  If we were to have this, we could do away
-- with the constraint of requiring instances to supply a dummy value for
-- InstrumentChannels when it isn't needed by providing two method signatures
-- with toMelody being implemented by (say) toMelodyWithLookup.

import Audio.SoundFont (InstrumentChannels)
import Audio.SoundFont.Melody (Melody, PMelody(..))
import Audio.SoundFont.Melody.Maker (toMelody) as MM
import Data.Midi as Midi

class Playable p where
  toMelody :: p -> InstrumentChannels -> Melody

-- the trivial melody instance ignores InstrumentChannels
instance playableMelody :: Playable PMelody where
  toMelody (PMelody melody) _ = melody

newtype MidiRecording = MidiRecording Midi.Recording

-- the midi instance also ignores InstrumentChannels
-- (channel numbers are already present in the MIDI)
instance playableMidi :: Playable MidiRecording where
  toMelody (MidiRecording recording) _ = MM.toMelody recording
