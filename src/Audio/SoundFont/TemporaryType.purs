module Audio.SoundFont.TemporaryType (MidiPitch) where

-- purescript-midi currently uses the type Note to define the pich of a note which is a simple Int type
-- in forthcoming versions it will use instead a newtype:  MidiPitch.  As a transitional step, we will
-- define MidiPitch temporarily here as an Int.  We will remove this once we integrate with the forthcoming ps-midi.
type MidiPitch = Int