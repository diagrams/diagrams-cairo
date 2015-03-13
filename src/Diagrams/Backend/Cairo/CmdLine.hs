{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo.CmdLine
-- Copyright   :  (c) 2013 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the cairo backend.
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'animMain' is like 'defaultMain' but for animations instead of
--   diagrams.
--
-- * `gifMain` creates an executable to generate an animated GIF.
--
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a suitable final result
--   (any of arguments to the above types) and 'Parseable' arguments.
--
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * Use a function with 'mainWith'.  This may require making
--   'Parseable' instances for custom argument types.
--
-- * Make a new 'Mainable' instance.  This may require a newtype
--   wrapper on your diagram type to avoid the existing instances.
--   This gives you more control over argument parsing, intervening
--   steps, and diagram creation.
--
-- * Build option records and pass them along with a diagram to 'mainRender'
--   from "Diagrams.Backend.CmdLine".
--
-- * A more flexible approach is to use the 'renderCairo' function
--   provided in the "Diagrams.Backend.Cairo" module.
--
-- * For the most flexibility, you can call the generic 'renderDia'
--   function directly; see "Diagrams.Backend.Cairo" for more
--   information.
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Cairo.CmdLine
       (
         -- * General form of @main@
         -- $mainwith

         mainWith

         -- * Supported forms of @main@

       , defaultMain
       , multiMain
       , animMain
       , gifMain

        -- * GIF support

        , GifOpts(..)
        , gifRender

        -- * Backend tokens

       , Cairo
       , B
       ) where

import           Codec.Picture
import           Codec.Picture.ColorQuant        (defaultPaletteOptions)
import qualified Data.ByteString.Lazy            as L (ByteString, writeFile)
import           Data.Vector.Storable            (unsafeFromForeignPtr0)
import           Data.Word                       (Word8)
import           Foreign.ForeignPtr.Safe         (ForeignPtr)
import           Options.Applicative

import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Ptr      (renderForeignPtrOpaque)
import           Diagrams.Backend.CmdLine
import           Diagrams.Prelude                hiding (height, interval,
                                                  option, output, width, (<>))

-- Below hack is needed because GHC 7.0.x has a bug regarding export
-- of data family constructors; see comments in Diagrams.Backend.Cairo
#if __GLASGOW_HASKELL__ < 702 || __GLASGOW_HASKELL__ >= 704
import           Diagrams.Backend.Cairo.Internal
#endif

import           Data.List.Split

-- $mainwith
-- The 'mainWith' method unifies all of the other forms of @main@ and is now
-- the recommended way to build a command-line diagrams program.  It works as a
-- direct replacement for 'defaultMain', 'multiMain', or 'animMain' as well as
-- allowing more general arguments.  For example, given a function that
-- produces a diagram when given an @Int@ and a @'Colour' Double@, 'mainWith'
-- will produce a program that looks for additional number and color arguments.
--
-- > ... definitions ...
-- > f :: Int -> Colour Double -> Diagram Cairo
-- > f i c = ...
-- >
-- > main = mainWith f
--
-- We can run this program as follows:
--
-- > $ ghc --make MyDiagram
-- >
-- > # output image.png built by `f 20 red`
-- > $ ./MyDiagram -o image.png -w 200 20 red


-- | This is the simplest way to render diagrams, and is intended to
--   be used like so:
--
-- > ... other definitions ...
-- > myDiagram = ...
-- >
-- > main = defaultMain myDiagram
--
--   Compiling a source file like the above example will result in an
--   executable which takes command-line options for setting the size,
--   output file, and so on, and renders @myDiagram@ with the
--   specified options.
--
--   On Unix systems, the generated executable also supports a
--   rudimentary \"looped\" mode, which watches the source file for
--   changes and recompiles itself on the fly.
--
--   Pass @--help@ to the generated executable to see all available
--   options.  Currently it looks something like
--
-- @
-- ./Program
--
-- Usage: ./Program [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT]
--                  [--loop] [-s|--src ARG] [-i|--interval INTERVAL]
--   Command-line diagram generation.
--
-- Available options:
--   -?,--help                Show this help text
--   -w,--width WIDTH         Desired WIDTH of the output image
--   -h,--height HEIGHT       Desired HEIGHT of the output image
--   -o,--output OUTPUT       OUTPUT file
--   -l,--loop                Run in a self-recompiling loop
--   -s,--src ARG             Source file to watch
--   -i,--interval INTERVAL   When running in a loop, check for changes every INTERVAL seconds.
-- @
--
--   For example, a couple common scenarios include
--
-- @
-- $ ghc --make MyDiagram
--
--   # output image.png with a width of 400px (and auto-determined height)
-- $ ./MyDiagram -o image.png -w 400
--
--   # output 200x200 dia.pdf, then watch for changes every 10 seconds
-- $ ./MyDiagram -o dia.pdf -h 200 -w 200 -l -i 10
-- @

defaultMain :: QDiagram Cairo V2 Double Any -> IO ()
defaultMain = mainWith

instance Mainable (QDiagram Cairo V2 Double Any) where
    type MainOpts (QDiagram Cairo V2 Double Any) = (DiagramOpts, DiagramLoopOpts)

    mainRender (opts, l) d = chooseRender opts d >> defaultLoopRender l

chooseRender :: DiagramOpts -> QDiagram Cairo V2 Double Any -> IO ()
chooseRender opts d =
  case splitOn "." (opts ^. output) of
    [""] -> putStrLn "No output file given."
    ps | last ps `elem` ["png", "ps", "pdf", "svg"] -> do
           let outTy = case last ps of
                 "png" -> PNG
                 "ps"  -> PS
                 "pdf" -> PDF
                 "svg" -> SVG
                 _     -> PDF
           fst $ renderDia
                   Cairo
                   ( CairoOptions
                     (opts^.output)
                     (fromIntegral <$> mkSizeSpec2D
                       (opts ^. width )
                       (opts ^. height)
                     )
                     outTy
                     False
                   )
                   d
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

-- | @multiMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams paired with names as input.
--   The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The
--   list of available diagrams may also be printed by passing the
--   option @--list@.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiTest
-- [1 of 1] Compiling Main             ( MultiTest.hs, MultiTest.o )
-- Linking MultiTest ...
-- $ ./MultiTest --list
-- Available diagrams:
--   foo bar
-- $ ./MultiTest --selection bar -o Bar.png -w 200
-- @

multiMain :: [(String, QDiagram Cairo V2 Double Any)] -> IO ()
multiMain = mainWith

instance Mainable [(String, QDiagram Cairo V2 Double Any)] where
    type MainOpts [(String, QDiagram Cairo V2 Double Any)]
        = (MainOpts (QDiagram Cairo V2 Double Any), DiagramMultiOpts)

    mainRender = defaultMultiMainRender

-- | @animMain@ is like 'defaultMain', but renders an animation
-- instead of a diagram.  It takes as input an animation and produces
-- a command-line program which will crudely \"render\" the animation
-- by rendering one image for each frame, named by extending the given
-- output file name by consecutive integers.  For example if the given
-- output file name is @foo\/blah.png@, the frames will be saved in
-- @foo\/blah001.png@, @foo\/blah002.png@, and so on (the number of
-- padding digits used depends on the total number of frames).  It is
-- up to the user to take these images and stitch them together into
-- an actual animation format (using, /e.g./ @ffmpeg@).
--
--   Of course, this is a rather crude method of rendering animations;
--   more sophisticated methods will likely be added in the future.
--
-- The @--fpu@ option can be used to control how many frames will be
-- output for each second (unit time) of animation.
animMain :: Animation Cairo V2 Double -> IO ()
animMain = mainWith

instance Mainable (Animation Cairo V2 Double) where
    type MainOpts (Animation Cairo V2 Double) = ((DiagramOpts, DiagramAnimOpts), DiagramLoopOpts)

    mainRender (opts, l) d =  defaultAnimMainRender chooseRender output opts d >> defaultLoopRender l

-- | @gifMain@ takes a list of diagram and delay time pairs and produces a
--   command line program to generate an animated GIF, with options @GifOpts@.
--   "Delay times are in 1/100ths of a second."
--
--   Example usage:
--
-- @
--   $ ghc --make GifTest
--   [1 of 1] Compiling Main             ( GifTest.hs, GifTest.o )
--   Linking GifTest ...
--   ./GifTest --help
--   GifTest
--
--   Usage: GifTest [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT]
--   [--dither] [--looping-off] [--loop-repeat ARG]
--   Command-line diagram generation.
--
--   Available options:
--    -?,--help                Show this help text
--    -w,--width WIDTH         Desired WIDTH of the output image
--    -h,--height HEIGHT       Desired HEIGHT of the output image
--    -o,--output OUTPUT       OUTPUT file
--    --dither                 Turn on dithering.
--    --looping-off            Turn looping off
--    --loop-repeat ARG        Number of times to repeat
-- @
gifMain :: [(QDiagram Cairo V2 Double Any, GifDelay)] -> IO ()
gifMain = mainWith

-- | Extra options for animated GIFs.
data GifOpts = GifOpts { _dither     :: Bool
                       , _noLooping  :: Bool
                       , _loopRepeat :: Maybe Int}

makeLenses ''GifOpts

-- | Command line parser for 'GifOpts'.
--   @--dither@ turn dithering on.
--   @--looping-off@ turn looping off, i.e play GIF once.
--   @--loop-repeat@ number of times to repeat the GIF after the first playing.
--   this option is only used if @--looping-off@ is not set.
instance Parseable GifOpts where
  parser = GifOpts <$> switch
                       ( long "dither"
                      <> help "Turn on dithering." )
                   <*> switch
                       ( long "looping-off"
                      <> help "Turn looping off" )
                   <*> ( optional . option auto )
                       ( long "loop-repeat"
                      <> help "Number of times to repeat" )

instance Mainable [(QDiagram Cairo V2 Double Any, GifDelay)] where
    type MainOpts [(QDiagram Cairo V2 Double Any, GifDelay)] = (DiagramOpts, GifOpts)

    mainRender (dOpts, gOpts) ds = gifRender (dOpts, gOpts) ds

imageRGB8FromUnsafePtr :: Int -> Int -> ForeignPtr Word8 -> Image PixelRGB8
imageRGB8FromUnsafePtr w h ptr = pixelMap f cImg
  where
    f (PixelRGBA8 b g r _) = PixelRGB8 r g b
    cImg = Image w h $ unsafeFromForeignPtr0 ptr (w * h * 4)


encodeGifAnimation' :: [GifDelay] -> GifLooping -> Bool
                   -> [Image PixelRGB8] -> Either String L.ByteString
encodeGifAnimation' delays looping dithering lst =
    encodeGifImages looping triples
      where
        triples = zipWith (\(x,z) y -> (x, y, z)) doubles delays
        doubles = [(pal, img)
                | (img, pal) <- palettize
                   defaultPaletteOptions {enableImageDithering=dithering} <$> lst]

writeGifAnimation' :: FilePath -> [GifDelay] -> GifLooping -> Bool
                  -> [Image PixelRGB8] -> Either String (IO ())
writeGifAnimation' path delays looping dithering img =
    L.writeFile path <$> encodeGifAnimation' delays looping dithering img

scaleInt :: Int -> Double -> Double -> Int
scaleInt i num denom
  | num == 0 || denom == 0 = i
  | otherwise = round (num / denom * fromIntegral i)

gifRender :: (DiagramOpts, GifOpts) -> [(QDiagram Cairo V2 Double Any, GifDelay)] -> IO ()
gifRender (dOpts, gOpts) lst =
  case splitOn "." (dOpts^.output) of
    [""] -> putStrLn "No output file given"
    ps | last ps == "gif" -> do
           let (w, h) = case (dOpts^.width, dOpts^.height) of
                          (Just w', Just h') -> (w', h')
                          (Just w', Nothing) -> (w', scaleInt w' diaHeight diaWidth)
                          (Nothing, Just h') -> (scaleInt h' diaWidth diaHeight, h')
                          (Nothing, Nothing) -> (100, 100)
               looping = if gOpts^.noLooping
                         then LoopingNever
                         else case gOpts^.loopRepeat of
                                Nothing -> LoopingForever
                                Just n  -> LoopingRepeat (fromIntegral n)
               dias = map fst lst
               delays = map snd lst
               V2 diaWidth diaHeight = size (head dias)
           fPtrs <- mapM (renderForeignPtrOpaque w h) dias
           let imageRGB8s = map (imageRGB8FromUnsafePtr w h) fPtrs
               result = writeGifAnimation'
                           (dOpts^.output)
                            delays
                            looping
                           (gOpts^.dither)
                            imageRGB8s
           case result of
             Left s   -> putStrLn s
             Right io -> io
       | otherwise -> putStrLn "File name must end with .gif"
