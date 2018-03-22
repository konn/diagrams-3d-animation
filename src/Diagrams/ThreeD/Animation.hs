{-# LANGUAGE FlexibleContexts, GADTs, NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures, TypeFamilies                #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Diagrams.ThreeD.Animation
  ( isometricFilm
  , rotatingAroundZ
  , animatedPlot
  , scatter3DWithLabel
  ) where
import Diagrams.LinearMap
import Diagrams.Prelude
import Diagrams.ThreeD.Projection
import Linear.Epsilon             (Epsilon)

type ActiveCamera n = Active (P3 n, Direction V3 n)

isometricFilm :: (Floating n, Epsilon n)
              => P3 n -> Active (P3 n, Direction V3 n) -> P3 n -> Active (P2 n)
isometricFilm orig cam space =
  let camera = uncurry (`lookingAt` orig) <$> cam
  in flip amap space <$> camera

-- | Camera looking at origin, rotating around the z-axis with specified
--   radius and height. It rotates in unit-time.
rotatingAroundZ :: (Floating n)
                => n            -- ^ Radius
                -> n            -- ^ Height
                -> ActiveCamera n
rotatingAroundZ r h =
  mkActive 0 1 $ \t ->
  let rt = fromRational $ fromTime t
      (x,y) = (r * cos (rt * 2 * pi), r * sin (rt * 2* pi))
      vec   = V3 x y h
  in (P vec , direction (- vec))

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (a, b, c) = f a b c

animatedPlot :: (Ord n, Semigroup m, Floating n, Epsilon n, Traversable t)
             => ActiveCamera n
             -> (a -> QDiagram b V2 n m)
             -> t (a, (n,n,n))
             -> QAnimation b V2 n m
animatedPlot cam r =
  fmap (foldMap (\(a, pos) -> r a # moveTo pos)) .
  traverse (uncurry $ \a -> fmap ((,) a) . isometricFilm origin cam . P . uncurry3 V3)

text' :: _ => Double -> String -> Diagram b
text' n str = text str # fontSizeL n <> strutY (n * 1.3)

renderScatter3D :: _ => String -> Diagram b
renderScatter3D lab =
  mconcat [ text' 5 lab
          , circle 5 # fc red
          ]

scatter3DWithLabel :: (Foldable t, _) => t (String, (Double, Double, Double))
                   -> QAnimation b V2 Double Any
scatter3DWithLabel lpts =
  let r = maximum $ fmap (\(_,(x,y,z)) -> sqrt (x*x + y*y + z*z)) lpts
  in animatedPlot (rotatingAroundZ (r * 1.5) (r * 1.5)) renderScatter3D lpts
