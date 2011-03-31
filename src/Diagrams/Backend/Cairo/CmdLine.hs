{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo.CmdLine
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Cairo backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Cairo.CmdLine
       ( defaultMain
       , multiMain

       , Cairo
       ) where

import Diagrams.Prelude hiding (width, height)
import Diagrams.Backend.Cairo

import System.Console.CmdArgs.Implicit

import Prelude hiding      (catch)

import Control.Applicative ((<$>))
import Control.Monad       (when, forever)
import Data.List.Split
import Data.Monoid

import System.Environment  (getArgs, getProgName)
import System.Directory    (getModificationTime)
import System.Process      (runProcess, waitForProcess)
import System.IO           (openFile, hClose, IOMode(..),
                            hSetBuffering, BufferMode(..), stdout)
import System.Exit         (ExitCode(..))
import System.Time         (ClockTime, getClockTime)
import Control.Concurrent  (threadDelay)
import Control.Exception   (catch, SomeException(..), bracket)

import System.Posix.Process (executeFile)

data DiagramOpts = DiagramOpts
                   { width     :: Int
                   , height    :: Int
                   , output    :: FilePath
                   , selection :: Maybe String
                   , loop      :: Bool
                   , interval  :: Int
                   }
  deriving (Show, Data, Typeable)

diagramOpts prog sel = DiagramOpts
  { width =  100
             &= typ "INT"
             &= help "Desired width of the output image"

  , height = 100
              &= typ "INT"
              &= help "Desired height of the output image"

  , output = def
           &= typFile
           &= help "Output file"

  , selection = def
              &= help "Name of the diagram to render"
              &= (if sel then typ "NAME" else ignore)

  , loop = False
            &= help "Run in a self-recompiling loop"

  , interval = 1 &= typ "SECONDS"
                 &= help "When running in a loop, check for changes every n seconds."
  }
  &= summary "Command-line diagram generation."
  &= program prog

defaultMain :: Diagram Cairo R2 -> IO ()
defaultMain d = do
  prog <- getProgName
  args <- getArgs
  opts <- cmdArgs (diagramOpts prog False)
  chooseRender opts d
  when (loop opts) (waitForChange Nothing opts prog args)

chooseRender :: DiagramOpts -> Diagram Cairo R2 -> IO ()
chooseRender opts d = do
  case splitOn "." (output opts) of
    [""] -> putStrLn "No output file given."
    ps | last ps `elem` ["png", "ps", "pdf", "svg"] -> do
           let outfmt = case last ps of
                 "png" -> PNG (width opts, height opts)
                 "ps"  -> PS  (fromIntegral $ width opts, fromIntegral $ height opts)
                 "pdf" -> PDF (fromIntegral $ width opts, fromIntegral $ height opts)
                 "svg" -> SVG (fromIntegral $ width opts, fromIntegral $ height opts)
           renderDia Cairo (CairoOptions (output opts) outfmt) d
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

multiMain :: [(String, Diagram Cairo R2)] -> IO ()
multiMain ds = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog True)
  case selection opts of
    Nothing  -> putStrLn "No diagram selected."
    Just sel -> case lookup sel ds of
      Nothing -> putStrLn $ "Unknown diagram: " ++ sel
      Just d  -> chooseRender opts d

waitForChange :: Maybe ClockTime -> DiagramOpts -> String -> [String] -> IO ()
waitForChange lastAttempt opts prog args = do
    hSetBuffering stdout NoBuffering
    go lastAttempt
  where go lastAttempt = do
          threadDelay (floor 1e6 * interval opts)
          -- putStrLn $ "Checking... (last attempt = " ++ show lastAttempt ++ ")"
          (newBin, newAttempt) <- recompile lastAttempt prog
          if newBin
            then executeFile prog False args Nothing
            else go $ getFirst (First newAttempt <> First lastAttempt)

-- | @recompile t prog@ attempts to recompile @prog@, assuming the
--   last attempt was made at time @t@.  If @t@ is @Nothing@ assume
--   the last attempt time is the same as the modification time of the
--   binary.  If the source file modification time is later than the
--   last attempt time, then attempt to recompile, and return the time
--   of this attempt.  Otherwise (if nothing has changed since the
--   last attempt), return @Nothing@.  Also return a Bool saying
--   whether a successful recompilation happened.
recompile :: Maybe ClockTime -> String -> IO (Bool, Maybe ClockTime)
recompile lastAttempt prog = do
  let errFile = prog ++ ".errors"
      srcFile = prog ++ ".hs"          -- XXX make this into an option?
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

      curTime <- getClockTime
      return (status == ExitSuccess, Just curTime)

    else return (False, Nothing)

 where getModTime f = catch (Just <$> getModificationTime f)
                            (\(SomeException _) -> return Nothing)