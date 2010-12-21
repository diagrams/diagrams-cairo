{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo.CmdLine
-- Copyright   :  (c) 2010 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Cairo backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Cairo.CmdLine where

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import System.Console.CmdArgs.Implicit

data DiagramOpts = DiagramOpts
                   { outWidth  :: Int
                   , outHeight :: Int
                   , output    :: FilePath
                   , selection :: Maybe String
                   }
  deriving (Show, Data, Typeable)

diagramOpts = DiagramOpts
  { outWidth =  100
             &= name "width"
             &= typ "INT"
             &= help "Desired width of the output image"

  , outHeight = 100
              &= name "height"
              &= typ "INT"
              &= help "Desired height of the output image"

  , output = def
           &= typFile
           &= help "Output file"

  , selection = def
              &= typ "NAME"
              &= help "Name of the diagram to render"
  }

defaultMain :: Diagram Cairo -> IO ()
defaultMain = undefined

multiMain :: [(String, Diagram Cairo)] -> IO ()
multiMain = undefined