{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a subtable final result
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

        -- * Backend tokens

       , Cairo
       , B
       ) where

import Control.Monad                       ((<=<))
import Codec.Picture                      (pixelMap, writeGifAnimation
                                         , GifLooping( .. ), PixelRGB8)
import Codec.Picture.Types                (dropAlphaLayer)
import Codec.Picture.VectorByteConversion (imageFromUnsafePtr)

import Control.Lens        ((^.),Lens')

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Ptr (renderForeignPtr)
import Diagrams.Backend.CmdLine

-- Below hack is needed because GHC 7.0.x has a bug regarding export
-- of data family constructors; see comments in Diagrams.Backend.Cairo
#if __GLASGOW_HASKELL__ < 702 || __GLASGOW_HASKELL__ >= 704
import Diagrams.Backend.Cairo.Internal
#endif

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding      (catch)
#else
import Prelude
#endif

import Data.List.Split

#ifdef CMDLINELOOP
import Data.Maybe          (fromMaybe)
import Control.Monad       (when, mplus)
import Control.Lens        (_1)

import System.Environment  (getArgs, getProgName)
import System.Directory    (getModificationTime)
import System.Process      (runProcess, waitForProcess)
import System.IO           (openFile, hClose, IOMode(..),
                            hSetBuffering, BufferMode(..), stdout)
import System.Exit         (ExitCode(..))
import Control.Concurrent  (threadDelay)
import Control.Exception   (catch, SomeException(..), bracket)

import System.Posix.Process (executeFile)
#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock (UTCTime,getCurrentTime)
type ModuleTime = UTCTime
getModuleTime :: IO  ModuleTime
getModuleTime = getCurrentTime
#else
import System.Time         (ClockTime, getClockTime)
type ModuleTime = ClockTime
getModuleTime :: IO  ModuleTime
getModuleTime = getClockTime
#endif
#endif

-- $mainwith
-- The 'mainWith' method unifies all of the other forms of @main@ and is now
-- the recommended way to build a command-line diagrams program.  It works as a
-- direct replacement for 'defaultMain', 'multiMain', or 'animMain' as well as
-- allowing more general arguments.  For example, given a function that
-- produces a diagram when given an @Int@ and a @'Colour' Double@, 'mainWith'
-- will produce a program that looks for additional number and color arguments.
--
-- > ... definitions ...
-- > f :: Int -> Colour Double -> Diagram Cairo R2
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

defaultMain :: Diagram Cairo R2 -> IO ()
defaultMain = mainWith

#ifdef CMDLINELOOP
output' :: Lens' (MainOpts (Diagram Cairo R2)) FilePath
output' = _1 . output

instance Mainable (Diagram Cairo R2) where
    type MainOpts (Diagram Cairo R2) = (DiagramOpts, DiagramLoopOpts)

    mainRender (opts,loopOpts) d = do
        chooseRender opts d
        when (loopOpts^.loop) (waitForChange Nothing loopOpts)
#else
output' :: Lens' (MainOpts (Diagram Cairo R2)) FilePath
output' = output

instance Mainable (Diagram Cairo R2) where
    type MainOpts (Diagram Cairo R2) = DiagramOpts

    mainRender opts d = chooseRender opts d
#endif

chooseRender :: DiagramOpts -> Diagram Cairo R2 -> IO ()
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
                     (mkSizeSpec
                       (fromIntegral <$> opts ^. width )
                       (fromIntegral <$> opts ^. height)
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

multiMain :: [(String, Diagram Cairo R2)] -> IO ()
multiMain = mainWith

instance Mainable [(String, Diagram Cairo R2)] where
    type MainOpts [(String, Diagram Cairo R2)]
        = (MainOpts (Diagram Cairo R2), DiagramMultiOpts)

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
animMain :: Animation Cairo R2 -> IO ()
animMain = mainWith

instance Mainable (Animation Cairo R2) where
    type MainOpts (Animation Cairo R2) = (MainOpts (Diagram Cairo R2), DiagramAnimOpts)

    mainRender = defaultAnimMainRender output'

-- [Diagram Cairo R2] is temporary, just to test the concept. Eventually we will
-- probably wrap in a newtype and may incude a delay list or delay list can be
-- command line options.
gifMain :: [Diagram Cairo R2] -> IO ()
gifMain = mainWith

instance Mainable [Diagram Cairo R2] where
    type MainOpts [Diagram Cairo R2] = DiagramOpts

    mainRender opts ds = gifRender opts ds


gifRender :: DiagramOpts -> [Diagram Cairo R2] -> IO ()
gifRender dOpts ds =
  case splitOn "." (dOpts^.output) of
    [""] -> putStrLn "No output file given"
    ps | last ps == "gif" -> do
           let Dims w' h' = mkSizeSpec (fromIntegral <$> dOpts ^. width )
                                       (fromIntegral <$> dOpts ^. height)
               (w, h) = (round w', round h')
           fPtrs <- mapM (renderForeignPtr w h) ds
           let imageRGB8s = map (imageFromUnsafePtr w h) fPtrs
               -- I would thing that the above image would be of type
               -- Image PixelRGBA8, i.e have alpha ?
               result = writeGifAnimation (dOpts^.output) 5 LoopingForever imageRGB8s
           case result of
             Left s   -> putStrLn s
             Right io -> io
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

#ifdef CMDLINELOOP
waitForChange :: Maybe ModuleTime -> DiagramLoopOpts -> IO ()
waitForChange lastAttempt opts = do
    prog <- getProgName
    args <- getArgs
    hSetBuffering stdout NoBuffering
    go prog args lastAttempt
  where go prog args lastAtt = do
          threadDelay (1000000 * opts^.interval)
          -- putStrLn $ "Checking... (last attempt = " ++ show lastAttempt ++ ")"
          (newBin, newAttempt) <- recompile lastAtt prog (opts^.src)
          if newBin
            then executeFile prog False args Nothing
            else go prog args $ newAttempt `mplus` lastAtt

-- | @recompile t prog@ attempts to recompile @prog@, assuming the
--   last attempt was made at time @t@.  If @t@ is @Nothing@ assume
--   the last attempt time is the same as the modification time of the
--   binary.  If the source file modification time is later than the
--   last attempt time, then attempt to recompile, and return the time
--   of this attempt.  Otherwise (if nothing has changed since the
--   last attempt), return @Nothing@.  Also return a Bool saying
--   whether a successful recompilation happened.
recompile :: Maybe ModuleTime -> String -> Maybe String -> IO (Bool, Maybe ModuleTime)
recompile lastAttempt prog mSrc = do
  let errFile = prog ++ ".errors"
      srcFile = fromMaybe (prog ++ ".hs") mSrc
  binT <- maybe (getModTime prog) (return . Just) lastAttempt
  srcT <- getModTime srcFile
  if (srcT > binT)
    then do
      putStr "Recompiling..."
      status <- bracket (openFile errFile WriteMode) hClose $ \h ->
        waitForProcess =<< runProcess "ghc" ["--make", srcFile]
                           Nothing Nothing Nothing Nothing (Just h)

      if (status /= ExitSuccess)
        then putStrLn "" >> putStrLn (replicate 75 '-') >> readFile errFile >>= putStr
        else putStrLn "done."

      curTime <- getModuleTime
      return (status == ExitSuccess, Just curTime)

    else return (False, Nothing)

 where getModTime f = catch (Just <$> getModificationTime f)
                            (\(SomeException _) -> return Nothing)
#endif
