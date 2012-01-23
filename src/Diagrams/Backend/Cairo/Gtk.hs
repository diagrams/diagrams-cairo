{-# LANGUAGE CPP #-}
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

-- Below hack is needed because GHC 7.0.x has a bug regarding export
-- of data family constructors; see comments in Diagrams.Backend.Cairo
#if __GLASGOW_HASKELL__ < 702 || __GLASGOW_HASKELL__ >= 704
import Diagrams.Backend.Cairo.Internal
#endif

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
toGtkCoords :: Monoid' m => QDiagram Cairo R2 m -> QDiagram Cairo R2 m
toGtkCoords d = snd $
  adjustDia Cairo
            (CairoOptions "" Absolute (GTK (undefined :: DrawWindow) False))
            d

-- | Render a diagram to a DrawingArea, rescaling to fit the full area.
defaultRender :: Monoid' m => DrawingArea -> QDiagram Cairo R2 m -> IO ()
defaultRender da d = do
  (w,h) <- widgetGetSize da
  dw <- widgetGetDrawWindow da
  fst $ renderDia Cairo (CairoOptions "" (Dims (fromIntegral w) (fromIntegral h))
                                         (GTK dw False)
                        )
        d

-- | Render a diagram to a DrawableClass.  No rescaling or transformations
-- will be performed.
--
-- Typically the diagram will already have been transformed by `toGtkCoords`.
renderToGtk ::
  (DrawableClass dc, Monoid' m)
  => dc                     -- ^ widget to render onto
  -> QDiagram Cairo R2 m  -- ^ Diagram
  -> IO ()
renderToGtk dc d =
  fst $ renderDia Cairo (CairoOptions "" Absolute (GTK dc True)) d
