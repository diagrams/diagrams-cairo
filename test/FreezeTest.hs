import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = Diagram Cairo R2

s :: D
s = square

s' = square # lw 0.03
            # lc black

s'' = s' # freeze

d :: D
d = hcat' with { sep = 2 }
--  [ s' , s'  # scaleX 5, s'  # scaleY 5, s'  # scale 5
  [ s'', s'' # scaleX 5, s'' # scaleY 5, s'' # scale 5 ]
--  , s' , s'  # scaleX 5 # freeze, s' # scaleY 5 # freeze, s' # scale 5 # freeze]

c :: D
c = circle # scale 20  -- # lc black  -- this changes the line width!!!

cs = hcat' with { sep = 10 }
     [ c, c1, c1 # scale 2, c1 # freeze # scale 2
     , c1 # scaleY 3, c1 # freeze # scaleY 3 ]
  where c1 = c # lw 1

main = defaultMain (cs # centerXY # pad 1.1)