# Digital Audio Synthesis

This software is a basic "Digital Audio Synthesis" This type of software constructs a sound wave (music) from a description of the music (composition).

# Input Description

To create the song, the program requires three .txt files as inputs. 

The programme take three .txt files to create the song 

The first one, named `comp.txt` and he contains :
* tempo: An integer representing the tempo of the song in beats per minute (BPM).
* patternLength: An integer representing the length of each pattern in the song, measured in beats.

The second file, named `pistes.txt` and he contains :
* patterns: A list of pattern objects, where each pattern object contains a sequence of notes, durations, and instruments.

The third file, named `inst.txt` and he contains :
* instruments: A list of instrument objects, where each instrument object contains a unique identifier, the * instrument's name, and its audio waveform characteristics.

# Output Description

The program will generate the following output:

* output.wav: A WAV file containing the synthesized audio of the song.
* songData: A data structure containing the following information:
   - songLength: The total length of the song in seconds.
   - numPatterns: The total number of patterns used in the song.
   - numInstruments: The total number of instruments used in the song.
   - patterns: A list of pattern objects, similar to the input but with additional metadata, such as the pattern's start and end times within the song.
   - instruments: A list of instrument objects, similar to the input but with additional metadata, such as the instrument's usage count within the song.

# Important Note on Code Conventions

Please be aware that in the source code for this program, variable names, function names, and comments are primarily written in French. We understand that this might pose some challenges for users who are not familiar with the French language. However, We have ensured that the user documentation, such as this README, is written in English for broader accessibility.
