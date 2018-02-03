module Audio.SoundFont.Melody (MidiPhrase, Melody, PMelody(..)) where

import Audio.SoundFont (MidiNote)

-- | a Melody is the entity that is played by a MIDI player
type Melody = Array MidiPhrase

-- | we split up a melody into phrases to allow the player to be
-- | re-rendered after each phrase is played
type MidiPhrase = Array MidiNote

newtype PMelody = PMelody Melody
