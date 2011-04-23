-- Diagrams created for blog post at
--   http://mathlesstraveled.com/2011/04/14/triangular-number-equations-via-pictures-solutions/

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Colour hiding (atop)

type D = Diagram Cairo R2
type C = Colour Double

tri :: C -> Int -> D
tri c n = dots `atop` (strokeT edges # lc c # lw 0.2 # fc (c `withOpacity` 0.5))
  where rows = map (hcat' with { sep = 1 })
             . zipWith replicate [n,n-1..1]
             . repeat
             $ dot c
        dots = decorateTrail (rotateBy (1/6) edge) rows
        edge = fromOffsets . replicate (n-1) $ unitX # scale 3
        edges = close (edge <> rotateBy (1/3) edge <> rotateBy (2/3) edge)

dot c = circle
      # lw 0
      # fc c

rowSpc = height (rotateBy (1/6) $ strutY 1 :: D)

-- @row k n s c@ draws a row of k size-n triangles with color c,
-- separated by enough space for @s@ dots.
row k n s c = hcat' with {sep = 1 + 3*s} (replicate k (tri c n))

-- 3 T(n) + T(n-1) = T(2n)
law1 :: Int -> C -> C -> D
law1 n c1 c2 = law3 1 n c1 c2

-- 3T(n) + T(n+1) = T(2n+1)
law2 :: Int -> C -> C -> D
law2 n c1 c2 = top === strutY rowSpc === (base `atop` mid)
  where base = row 2 n 1 c1
             # centerX
             # alignBottom
        mid  = row 1 n 0 c1
             # reflectY
             # centerX
             # alignBottom
        top  = tri c2 (n+1)
             # centerX

-- (2k+1)T(n) + T(kn - 1) = T((k+1)n)
law3 :: Int -> Int -> C -> C -> D
law3 k n c1 c2 = top === strutY rowSpc === (mid `atop` base)
  where base = row (k+1) n 0 c1 # centerX
                                # alignBottom
        mid  = row k n 0 c1 # reflectY
                            # centerX
                            # alignBottom
                            # translateY (2 + rowSpc)
        top  = tri c2 (k*n - 1) # centerX

-- T(n) T(k) + T(n-1) T(k-1) = T(nk)
law4 :: Int -> Int -> C -> C -> D
law4 k n c1 c2 = vcat' with {sep = rowSpc} (map tRow [1..k])
  where tRow k = (row k n 0 c1 # centerX # alignTop)
                `atop`
                 (row (k-1) (n-1) 1 c2 # reflectY # centerX # alignTop)

exampleRow f = hcat' with {sep = 4} . map (alignBottom . f)

law1Dia :: D
law1Dia = exampleRow law1' [2..4]
  where law1' n = law1 n blue red

law2Dia :: D
law2Dia = exampleRow law2' [1..3]
  where law2' n = law2 n green orange

law3Dia :: D
law3Dia = exampleRow law3' [1..3]
  where law3' k = law3 k 2 saddlebrown gray

law4Dia :: D
law4Dia = exampleRow law4' [2..4]
  where law4' k = law4 k 3 purple gold

-- showOrigin = ((circle # fc red) `atop`)

main = defaultMain (pad 1.05 $ vcat' with {sep=5} . map centerXY $
                             [law1Dia, law2Dia, law3Dia, law4Dia])
