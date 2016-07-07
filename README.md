# spellcheck

A simple spellchecker for the command line.

## Installation

Most conveniently installed using stack.

Currently requires a file called "words" to be present in the root directory of the project during compilation. The file must contain a word each line. Those are the words against which this checker will match. It will consider all those words correct. An easy way to obtain such a file is the shell command `aspell dump master | aspell expand > words`.

## Usage

The spellchecker has two modi:

1. Interactive
    Continuously prompts you to input words which it will check and provide 4 spelling alternatives for.

2. Filecheck
    If the program is invoked with two command line arguments those will be treated as a path to an input file and a path to an output file respectively.
    The file at the input path will be read and word for word will be checked. If the program considers the word to be spelled incorrectly it will prompt the user with 4 alternatives at a time to correct for (expandable). The corrected file will be written to the output path.
