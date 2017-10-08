purescript-soundfonts
=====================

WORK IN PROGRESS

This is an experimental complete re-write of the soundfont library.  Although still based on, and strongly influenced by danigb's soundfont project: [soundfont-player](https://github.com/danigb/soundfont-player) it no longer has any dependency on it.  The idea is to write as much as possible in PureScript and only to use native JavaScript where absolutely necessary in order to invoke Web Audio functions.  These are required in two places:

*  When building an AudioBuffer for each note in an instruments SoundFont.
*  When playing a note that uses an AudioBuffer.

All that has been provided so far is a proof of concept.  A SoundFont can be loaded from Benjamin Gleitzman's package of [pre-rendered sound fonts](https://github.com/gleitz/midi-js-soundfonts), decoded and stored.  A succession of notes can then be played through the SoundFont buffers.

## Build

    bower install   
    pulp build
   
## Example

    ./build-example.sh

and then navigate to dist/index.html