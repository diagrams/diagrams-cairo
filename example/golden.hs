import Graphics.Rendering.Diagrams

import Diagrams.TwoD
import Diagrams.TwoD.Arc

import Diagrams.Backend.Cairo
import Diagrams.Path
import Diagrams.Segment
import Diagrams.Attributes
import Diagrams.Combinators

import Data.VectorSpace
import Data.Colour (withOpacity)
import Data.Colour.Names

import Data.Monoid (Any)

type D = AnnDiagram Cairo Any

square :: D
square = translate (-1/2,-1/2) $ b `atop` a
  where a = stroke . translate (1,0) $ arc (pi / 2) pi
        b = lw 0.5 . scale (1/2) . translate (1,1) $ box

phi :: Double
phi = (1 + sqrt 5) / 2

step :: D -> D -> D
step a b = besideAlign (-1,0) (0,1) a b'
  where b' = rotate (-pi/2) . scale (1/phi) $ b
  
besideAlign :: R2 -> R2 -> D -> D -> D
besideAlign u v a b = beside' u a b'
  where b' = translate (v ^* d) b
        (Bounds bb) = bounds b
        (Bounds ba) = bounds a
        d = (ba v - bb v)

-- TODO: figure out why this doesn't work the same as beside 
-- that is based on rebase.
beside' :: R2 -> D -> D -> D
beside' v a b = translate (ba v *^ v) a `atop` translate (bb (negateV v) *^ negateV v) b
  where (Bounds bb) = bounds b
        (Bounds ba) = bounds a

golden :: D
golden = foldr1 step (replicate 10 square)
  
main :: IO ()
main = renderDia Cairo opts golden
  where opts = CairoOptions "golden.png" $ PNG (400, 300)
