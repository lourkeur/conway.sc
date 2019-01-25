# Terminal-based Conway's Game Of Life Simulator
## Features
- Can be run with Lanterna's terminal emulator
- Pass arbitrary initial grid in RLE notation
- Infinite grid
- View can scroll

## Setup
Install [Ammonite,](https://ammonite.io/) then invoke `./conway` at the command line.
You should see the classic Gosper Glider Gun appear and start evolving.

## Usage
Try `./conway -p RPentomino` to see the famous methuselah,
or `./conway -p Copperhead` to see the (relatively) recently arrived spaceship.
Use `./conway 'b2o$o2bo$b2o'` to supply your own RLE pattern.
For more options, check the built-in help `./conway --help`.

## History
Originated from a [Python gist of mine](https://gist.github.com/lourkeur/95799b35e2d3aac54cdd0e4a7c8d2037) inspired by [this conference.](https://youtu.be/o9pEzgHorH0?t=1032)
