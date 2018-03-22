{-# LANGUAGE FlexibleContexts, GADTs, NoMonomorphismRestriction   #-}
{-# LANGUAGE PartialTypeSignatures, PatternSynonyms, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Diagrams.ThreeD.Animation
  ( isometricFilm
  , rotatingAroundZ
  , animatedPlot
  , scatter3DWithLabel
  ) where
import Debug.Trace
import Diagrams.LinearMap
import Diagrams.Prelude           hiding (trace)
import Diagrams.ThreeD.Projection
import Linear                     ((!*!))
import Linear.Epsilon             (Epsilon)

tr :: Show b => String -> b -> b
tr lab a = trace (lab <> show a) $! a

type ActiveCamera n = Active (P3 n, Direction V3 n)

isometricFilm :: (Floating n, Epsilon n, _)
              => P3 n -> Active (P3 n, Direction V3 n) -> a -> Active b
isometricFilm orig cam space =
  cam <&> \(e, d) ->
    let proj = lookingAt e orig d
    in amap proj space # moveOriginTo (amap proj orig)

perspectiveFilm :: (Floating n, Epsilon n, _)
                => Angle n -> P3 n -> Active (P3 n, Direction V3 n) -> a -> Active b
perspectiveFilm vision orig cam space =
  cam <&> \(P e, d) ->
   let standPoint = lookAt e zero (fromDir d)
       proj = m44Deformation $
              perspective (vision ^. rad) 0.8 1 3 !*! standPoint
   in deform proj space # moveOriginTo (deform proj orig)

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
  in (P vec , direction unitZ)

animatedPlot :: ( Ord n, Floating n, Epsilon n, Traversable t, _)
             => ActiveCamera n
             -> (a -> QDiagram b V2 n Any)
             -> t (a, (n,n,n))
             -> QAnimation b V2 n Any
animatedPlot cam r =
  fmap (foldMap (\(a, pos) -> r a # moveTo pos)) .
  traverse (uncurry $ \a -> fmap ((,) a) . perspectiveFilm (pi/3 @@ rad) origin cam . p3)

text' :: _ => Double -> String -> Diagram b
text' n str = text str # fontSizeG n <> strutY (n * 1.3)

pattern P3 :: x -> x -> x -> P3 x
pattern P3 x y z = P (V3 x y z)

pattern P2 :: x -> x -> P2 x
pattern P2 x y = P (V2 x y)

axes :: (Floating n, Ord n) => n -> Path V3 n
axes d = mconcat [ origin ~~ P3 d 0 0
                 , origin ~~ P3 0 d 0
                 , origin ~~ P3 0 0 d
                 ]

renderScatter3D :: _ => Double -> String -> Diagram b
renderScatter3D n lab = mconcat
  [
    text' n lab # moveTo (P2 (3*n/2) (3*n/2))
  , circle (n/2) # fc red # lcA transparent
  ]

pattern Exactly :: a -> Option a
pattern Exactly a = Option (Just a)

pattern None :: Option a
pattern None = Option Nothing

scatter3DWithLabel :: (Foldable t, _)
                   => t (String, (Double, Double, Double))
                   -> QAnimation b V2 Double Any
scatter3DWithLabel ls0 =
  let (rMax, lpts) =
        foldr (\(l,(x,y,z)) (u, ls) ->
                  (u `max` maximum (map abs [x,y,z])
                  , (l, (10 * x / rMax, 10 * y / rMax, 10 * z / rMax) ) : ls)
              )
          (1, []) ls0
      cam = rotatingAroundZ 10 10
      background = square 5 # fc white # lc white
  in foldr1 atop <$>
     sequenceA [ animatedPlot cam (renderScatter3D 0.1) lpts
               , lw 1 . fcA transparent . lc black . stroke <$> perspectiveFilm (pi/3 @@ rad) origin cam (axes 10)
               , background
               ]
