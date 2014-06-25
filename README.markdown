[![Build Status](https://travis-ci.org/diagrams/diagrams-cairo.png?branch=master)](http://travis-ci.org/diagrams/diagrams-cairo)

_diagrams-cairo_ is a rendering backend for [diagrams], a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

_diagrams-cairo_ is implemented using the [cairo] rendering engine and
is a fully-featured, officially supported backend for diagrams.

[cairo]: http://www.cairographics.org/

# Installation

```
cabal update && cabal install gtk2hs-buildtools diagrams-cairo
```

# Basic usage

A simple example that uses _diagrams-cairo_ to draw a blue circle:

```haskell
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = circle 1 # fc blue

main = mainWith (pad 1.1 d)
```

Save this to file named `Circle.hs` and compile it:

```
ghc --make Circle.hs
```

This will generate an executable which, when run, outputs a blue
circle to some file. Run the executable with the `--help` option to
find out more about how to call it.

```
$ ./Circle --help
./Circle

Usage: ./Circle [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT]
                [--loop] [-s|--src ARG] [-i|--interval INTERVAL]
  Command-line diagram generation.

Available options:
  -?,--help                Show this help text
  -w,--width WIDTH         Desired WIDTH of the output image
  -h,--height HEIGHT       Desired HEIGHT of the output image
  -o,--output OUTPUT       OUTPUT file
  -l,--loop                Run in a self-recompiling loop
  -s,--src ARG             Source file to watch
  -i,--interval INTERVAL   When running in a loop, check for changes every INTERVAL seconds.
ommand-line diagram generation.
```

The output type will be automatically determined from the file
extension.  Currently PNG, PDF, PS, and SVG are supported.

```
$ ./Circle -o circle.png -w 400
```

The command above generates a PNG file with a width of 400px.

# Advanced usage

Instead of just creating a standalone executable, the cairo backend
can also be called from within a larger program.  For more
information, see the Diagram.Backend.Cairo module.
