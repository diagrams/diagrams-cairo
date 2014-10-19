{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine
-- import Diagrams.Backend.Show

rule :: Diagram Cairo
rule = (strokeT $ fromOffsets [(1,0)])
--     # rotateBy (1/4)

main = defaultMain mempty -- rule
