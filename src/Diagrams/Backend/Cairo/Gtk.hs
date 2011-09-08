-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo.Gtk
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient interface to rendering diagrams directly
-- on Gtk widgets using the Cairo backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Cairo.Gtk
       ( defaultRender
       , toGtkCoords
       , renderToGtk
       ) where

import Diagrams.Prelude hiding (width, height)
import Diagrams.Backend.Cairo

import Graphics.UI.Gtk

-- | Convert a Diagram to the backend coordinates.
-- 
-- Provided to Query the diagram with coordinates from a mouse click
-- event.
-- 
-- > widget `on` buttonPressEvent $ tryEvent $ do
-- >   click <- eventClick
-- >   (x,y) <- eventCoordinates
-- >   let result = runQuery (query $ toGtkCoords myDiagram) (P (x,y))
-- >   do_something_with result
-- 
-- `toGtkCoords` does no rescaling of the diagram, however it is centered in
-- the window.
toGtkCoords :: Monoid m => AnnDiagram Cairo R2 m -> AnnDiagram Cairo R2 m
toGtkCoords d =
  adjustDia Cairo
            (CairoOptions "" (GTK (undefined :: DrawWindow) Nothing False))
            d

-- | Render a diagram to a DrawingArea, rescaling to fit the full area.
defaultRender :: Monoid m => DrawingArea -> AnnDiagram Cairo R2 m -> IO ()
defaultRender da d = do
  sz <- widgetGetSize da
  dw <- widgetGetDrawWindow da
  fst $ renderDia Cairo (CairoOptions "" (GTK dw (Just sz) False)) d

-- | Render a diagram to a DrawableClass.  No rescaling or transformations
-- will be performed.
-- 
-- Typically the diagram will already have been transformed by `toGtkCoords`.
renderToGtk ::
  (DrawableClass dc, Monoid m)
  => dc                     -- ^ widget to render onto
  -> AnnDiagram Cairo R2 m  -- ^ Diagram
  -> IO ()
renderToGtk dc d =
  fst $ renderDia Cairo (CairoOptions "" (GTK dc Nothing True)) d
