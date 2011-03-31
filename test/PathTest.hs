import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

p :: Trail R2
p = fromOffsets [(1,5), (1,-5)]

burst n = close . mconcat . take n . iterate (rotateBy (-1/(fromIntegral n))) $ p

sun = (strokeT $ burst 20)
      # lineJoin LineJoinRound
      # fc yellow
      # lc red
      # centerXY

dia = mconcat . reverse . take 8 . iterate (scale 0.8) $ sun

main = defaultMain dia