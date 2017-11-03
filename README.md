purescript-soundfonts
=====================

[![Build Status](https://travis-ci.org/newlandsvalley/purescript-soundfonts.svg?branch=master)](https://travis-ci.org/newlandsvalley/purescript-soundfonts)

This is an experimental complete re-write of the soundfont library.  Although still based on, and strongly influenced by danigb's soundfont project: [soundfont-player](https://github.com/danigb/soundfont-player) it no longer has any dependency on it.  The idea is to write as much as possible in PureScript and only to use native JavaScript where absolutely necessary in order to invoke Web Audio functions.  These are required in the following places:

*  When building an AudioBuffer for each note in an instrument SoundFont.
*  When playing a note that uses an AudioBuffer.
*  And in non-essential API functions canPlayOgg and isWebAudioEnabled.

A SoundFont can be loaded from Benjamin Gleitzman's package of [pre-rendered sound fonts](https://github.com/gleitz/midi-js-soundfonts) or from a resource on the local server. It is then decoded into a dictionary of Audio Buffers (one for each note).  A succession of notes can then be played through these buffers.

The conversion of a Gleitzman note name (e.g. Ab4) to a MIDI pitch uses middle C = C4.

This is intended to be functionally equivalent to purescript-polymorphic soundfonts.  The major difference from a user perspective is that the soundfonts are no longer implicit and have to be stored by the user and passed to the __playNote__ functions.  The rarely used function to get the current time from the Audio Context has been dropped.

## Build

    bower install   
    pulp build

## Example

    ./build-example.sh

host on your web server and then navigate to dist/index.html
