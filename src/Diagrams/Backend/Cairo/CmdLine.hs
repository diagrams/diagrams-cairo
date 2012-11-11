{-# LANGUAGE DeriveDataTypeable, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo.CmdLine
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
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
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * A simple but somewhat inflexible approach is to wrap up
--   'defaultMain' (or 'multiMain', or 'animMain') in a call to
--   'System.Environment.withArgs'.
--
-- * A more flexible approach is to directly call 'renderDia'; see
--   "Diagrams.Backend.Cairo" for more information.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Cairo.CmdLine
       ( defaultMain
       , multiMain
       , animMain

       , Cairo
       ) where

import Data.List (intercalate)
import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.Cairo

-- Below hack is needed because GHC 7.0.x has a bug regarding export
-- of data family constructors; see comments in Diagrams.Backend.Cairo
#if __GLASGOW_HASKELL__ < 702 || __GLASGOW_HASKELL__ >= 704
import Diagrams.Backend.Cairo.Internal
#endif

import System.Console.CmdArgs.Implicit hiding (args)

import Prelude hiding      (catch)

import Data.Maybe          (fromMaybe)
import Control.Monad       (when, forM_, mplus)
import Data.List.Split

import Text.Printf

import System.Environment  (getArgs, getProgName)
import System.Directory    (getModificationTime)
import System.FilePath     (addExtension, splitExtension)
import System.Process      (runProcess, waitForProcess)
import System.IO           (openFile, hClose, IOMode(..),
                            hSetBuffering, BufferMode(..), stdout)
import System.Exit         (ExitCode(..))
import Control.Concurrent  (threadDelay)
import Control.Exception   (catch, SomeException(..), bracket)

#ifdef CMDLINELOOP
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

data DiagramOpts = DiagramOpts
                   { width     :: Maybe Int
                   , height    :: Maybe Int
                   , output    :: FilePath
                   , list      :: Bool
                   , selection :: Maybe String
                   , fpu       :: Double
#ifdef CMDLINELOOP
                   , loop      :: Bool
                   , src       :: Maybe String
                   , interval  :: Int
#endif
                   }
  deriving (Show, Data, Typeable)

diagramOpts :: String -> Bool -> DiagramOpts
diagramOpts prog sel = DiagramOpts
  { width =  def
             &= typ "INT"
             &= help "Desired width of the output image"

  , height = def
             &= typ "INT"
             &= help "Desired height of the output image"

  , output = def
           &= typFile
           &= help "Output file"

  , selection = def
              &= help "Name of the diagram to render"
              &= (if sel then typ "NAME" else ignore)

  , list = def
         &= (if sel then help "List all available diagrams" else ignore)

  , fpu = 30
          &= typ "FLOAT"
          &= help "Frames per unit time (for animations)"
#ifdef CMDLINELOOP
  , loop = False
            &= help "Run in a self-recompiling loop"
  , src  = def
            &= typFile
            &= help "Source file to watch"
  , interval = 1 &= typ "SECONDS"
                 &= help "When running in a loop, check for changes every n seconds."
#endif
  }
  &= summary "Command-line diagram generation."
  &= program prog

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
-- Command-line diagram generation.
--
-- Foo [OPTIONS]
--
-- Common flags:
--   -w --width=INT         Desired width of the output image
--   -h --height=INT        Desired height of the output image
--   -o --output=FILE       Output file
--   -f --fpu=FLOAT         Frames per unit time (for animations)
--   -l --loop              Run in a self-recompiling loop
--   -s --src=FILE          Source file to watch
--   -i --interval=SECONDS  When running in a loop, check for changes every n
--                          seconds.
--   -? --help              Display help message
--   -V --version           Print version information
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
defaultMain d = do
  prog <- getProgName
  args <- getArgs
  opts <- cmdArgs (diagramOpts prog False)
  chooseRender opts d
#ifdef CMDLINELOOP
  when (loop opts) (waitForChange Nothing opts prog args)
#endif

chooseRender :: DiagramOpts -> Diagram Cairo R2 -> IO ()
chooseRender opts d =
  case splitOn "." (output opts) of
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
                     (output opts)
                     (mkSizeSpec
                       (fromIntegral <$> width opts)
                       (fromIntegral <$> height opts)
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
multiMain ds = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog True)
  if list opts
    then showDiaList (map fst ds)
    else
      case selection opts of
        Nothing  -> putStrLn "No diagram selected." >> showDiaList (map fst ds)
        Just sel -> case lookup sel ds of
          Nothing -> putStrLn $ "Unknown diagram: " ++ sel
          Just d  -> chooseRender opts d

-- | Display the list of diagrams available for rendering.
showDiaList :: [String] -> IO ()
showDiaList ds = do
  putStrLn "Available diagrams:"
  putStrLn $ "  " ++ intercalate " " ds

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
animMain anim = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog False)
  let frames  = simulate (toRational $ fpu opts) anim
      nDigits = length . show . length $ frames
  forM_ (zip [1..] frames) $ \(i,d) ->
    chooseRender (indexize nDigits i opts) d

-- | @indexize d n@ adds the integer index @n@ to the end of the
--   output file name, padding with zeros if necessary so that it uses
--   at least @d@ digits.
indexize :: Int -> Integer -> DiagramOpts -> DiagramOpts
indexize nDigits i opts = opts { output = output' }
  where fmt         = "%0" ++ show nDigits ++ "d"
        output'     = addExtension (base ++ printf fmt (i::Integer)) ext
        (base, ext) = splitExtension (output opts)

#ifdef CMDLINELOOP
waitForChange :: Maybe ModuleTime -> DiagramOpts -> String -> [String] -> IO ()
waitForChange lastAttempt opts prog args = do
    hSetBuffering stdout NoBuffering
    go lastAttempt
  where go lastAtt = do
          threadDelay (1000000 * interval opts)
          -- putStrLn $ "Checking... (last attempt = " ++ show lastAttempt ++ ")"
          (newBin, newAttempt) <- recompile lastAtt prog (src opts)
          if newBin
            then executeFile prog False args Nothing
            else go $ newAttempt `mplus` lastAtt

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
