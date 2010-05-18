{-# LANGUAGE TypeFamilies #-}
module Diagrams.Backend.Cairo where

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Diagrams.Backends

import Diagrams.TwoD

data Cairo = Cairo

instance Backend Cairo where
  type BSpace Cairo = P2
  type Render Cairo = C.Render
  runRender _ r = C.withPDFSurface "test.pdf" 100 100 $ \surface ->
                    C.renderWith surface r

instance Renderable Box Cairo where
  render _ (Box v1 v2 v3 v4)