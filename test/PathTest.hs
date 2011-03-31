import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = AnnDiagram Cairo R2 Any

p :: Trail R2
p = fromOffsets [(1,2), (1,-5)]

burst n = close . mconcat . take n . iterate (rotateBy (-1/(fromIntegral n))) $ p

sun = (strokeT $ burst 25)
      # lineJoin LineJoinRound
      # fc yellow
      # lc red
      # lw 1
      # centerXY
      # lw 1

dia = mconcat . reverse . take 15 . iterate (scale 0.8) $ sun

pad d = withBounds (scale 1.1 d) d

main = defaultMain (pad dia)