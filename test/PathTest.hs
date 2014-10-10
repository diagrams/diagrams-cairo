import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = AnnDiagram Cairo V2 Double Any

p :: Trail V2 Double
p = fromOffsets [(1,2), (1,-5)]

burst n = close . mconcat . take n . iterate (rotateBy (-1/(fromIntegral n))) $ p

sun = (strokeT $ burst 25)
      # lineJoin LineJoinRound
      # fc yellow
      # lc red
      # lw 1
      # centerXY

dia = mconcat . reverse . take 15 . iterate (scale 0.8) $ sun

main = defaultMain (pad 1.1 dia)