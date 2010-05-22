{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Diagrams.Backend.Cairo where

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Diagrams

import Diagrams.TwoD

data Cairo = Cairo

instance Backend Cairo where
  type BSpace Cairo = P2
  type Render Cairo = C.Render ()
  type Result Cairo = IO ()
  data Option Cairo = OutputFile String  -- XXX add more options!

  renderDia _ _ d = C.withPDFSurface "test.pdf" 100 100 $ \surface ->
                    C.renderWith surface (mapM_ (render Cairo) (prims d))

instance Renderable Box Cairo where
  render _ (Box v1 v2 v3 v4) = do
    C.newPath
    uncurry C.moveTo v1
    uncurry C.lineTo v2
    uncurry C.lineTo v3
    uncurry C.lineTo v4
    C.closePath
    C.stroke