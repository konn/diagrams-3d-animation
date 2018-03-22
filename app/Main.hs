{-# LANGUAGE TupleSections #-}
module Main where
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.CmdLine
import Diagrams.Prelude
import Diagrams.ThreeD.Animation

dataSet :: [(String, (Double, Double, Double))]
dataSet = [("A", (1,-1,0)), ("B", (0, 1.5, 1)), ("C", (2, 0,-2))]

anime :: Animation Cairo V2 Double
anime = scatter3DWithLabel dataSet

toDias :: Duration Rational -> Animation b V2 Double -> [QDiagram b V2 Double Any]
toDias dur = simulate 2 . stretchTo dur

main :: IO ()
main = do
  let dOpt = DiagramOpts (Just 300) Nothing "anime.gif"
      gOpts = GifOpts False True Nothing
  gifRender (dOpt, gOpts) $ map (,10) $ toDias 30 anime

