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

import Data.List.Split

import System.Console.CmdArgs.Implicit
import System.Environment

data DiagramOpts = DiagramOpts
                   { width  :: Int
                   , height :: Int
                   , output    :: FilePath
                   , selection :: Maybe String
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
              &= typ "NAME"
              &= help "Name of the diagram to render"
              &= (if sel then typ "NAME" else ignore)
  }
  &= summary "Command-line diagram generation."
  &= program prog

defaultMain :: Diagram Cairo R2 -> IO ()
defaultMain d = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog False)
  chooseRender opts d

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