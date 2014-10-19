import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = Diagram Cairo

s :: D
s = unitSquare

s' = unitSquare
     # lw 0.03
     # lc black

s'' = s' # freeze

d :: D
d = hcat' with { sep = 2 }
  [ s' , s'  # scaleX 5, s'  # scaleY 5, s'  # scale 5
  , s'', s'' # scaleX 5, s'' # scaleY 5, s'' # scale 5 ]

c :: D
c = circle 20 # lc red

cs = hcat' with { sep = 10 }
     [ c, c1, c1 # scale 2, c1 # freeze # scale 2
     , c1 # scaleY 3, c1 # freeze # scaleY 3, c1 # scaleY 3 # freeze ]
  where c1 = c # lw 1

main = defaultMain (d # centerXY # pad 1.1)
       -- (cs # centerXY # pad 1.1)
