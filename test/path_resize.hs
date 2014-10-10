{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine
-- import Diagrams.Backend.Show

type D = Diagram Cairo V2 Double

rule :: D
rule = (strokeT $ fromOffsets [(1,0)])
--     # rotateBy (1/4)

main = defaultMain mempty -- rule