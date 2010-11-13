import Graphics.Rendering.Diagrams

import Diagrams.Backend.Cairo

import Diagrams.Attributes
import Diagrams.TwoD

import Data.Colour.Names

b1 = lc red $ fc blue $ box

opts = CairoOptions "attr.pdf" $ PDF (400, 400)
main = renderDia Cairo opts (translate (30,30) $ scale 15 $ b1)
