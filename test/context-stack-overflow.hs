{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

line = strokeT $ fromOffsets [(1,0)]

main = defaultMain line