[![Build Status](https://secure.travis-ci.org/diagrams/diagrams-cairo.png)](http://travis-ci.org/diagrams/diagrams-cairo)

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

main = defaultMain (pad 1.1 d)
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
Command-line diagram generation.

Circle [OPTIONS]

Common flags:
  -w --width=INT    Desired width of the output image
  -h --height=INT   Desired height of the output image
  -o --output=FILE  Output file
  -? --help         Display help message
  -V --version      Print version information
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
