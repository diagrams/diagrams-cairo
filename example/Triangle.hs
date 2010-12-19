import Diagrams.Prelude 
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Shapes

p :: Diagram Cairo
p = polygonDiagrams 3

bigP:: Diagram Cairo
bigP = scale 2 p

twoP :: Diagram Cairo
twoP = beside (1,0) bigP p

main = renderDia Cairo (CairoOptions "Triangle.pdf" (PDF (100.0,100.0))) twoP
