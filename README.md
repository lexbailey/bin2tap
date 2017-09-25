# bin2tap
Takes binary files, puts them in .tap files.
Handy for when you want to load binary data into zx spectrum emulators.
These can also be converted to audio to load on a real spectrum.

## Licence

Licenced with the MIT licence, see LICENCE file

## Building

Build using ghc like so...

`ghc bin2tap.hs -o bin2tap`

## Usage

`bin2tap address name file`

where:
 - `address` is the start address in the ZX spectrum memory, in the range 0x0000 to 0xFFFF
 - `name` is the name of the file on the tape (the name the spectrum will see, no more than 10 chars)
 - `file` is the name of the binary to convert

## Example

You have a binary file called `foo.dat` that you want to load at `0xFE00` and you want to `LOAD "datafoo"`

`bin2tap 0xFE00 datafoo foo.dat`
