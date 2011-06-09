{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.List.Split

letterBlock c = square 1.5 <> text [c]

main = defaultMain (hcat $ map letterBlock ['a'..'z'])