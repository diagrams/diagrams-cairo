import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = stroke $ pathFromOffsets (P (0,0)) [(1,1), (1,2), (1,3), (7,2)]

main = defaultMain d
