import Diagrams.Prelude

import Diagrams.Backend.Cairo

b :: Diagram Cairo
b = beside (1,1) (rotate (pi/4) $ scaleY 2 circle) square

main :: IO ()
main = renderDia Cairo opts b
  where opts = CairoOptions "beside.pdf" $ PDF (400,400)