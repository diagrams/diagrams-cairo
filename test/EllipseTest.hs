{-# LANGUAGE FlexibleInstances #-}
import Diagrams.TwoD.Ellipse

import Test.QuickCheck

import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Util

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform

import Control.Monad
import Control.Applicative

import Data.Monoid (Any(..), mempty, Monoid(..))

import Data.VectorSpace (magnitudeSq, (^-^))

data TStep = Rotate Double | ScaleX Double | ScaleY Double | TranslateX Double | TranslateY Double
    deriving (Show, Eq)

newtype TSteps = TSteps [TStep]
newtype PlusMinusTwoPi = PlusMinusTwoPi Double

data TEllipse = TEllipse [TStep] Ellipse

mkTransformation :: [TStep] -> Transformation V2 Double
mkTransformation = mconcat . map step
  where
    step (Rotate     a) = rotation a
    step (ScaleX     x) = scalingX x
    step (ScaleY     y) = scalingY y
    step (TranslateX x) = translationX x
    step (TranslateY y) = translationY y

instance Arbitrary PlusMinusTwoPi where
    arbitrary = PlusMinusTwoPi <$> choose (-2*pi, 2*pi)

instance Arbitrary TStep where
    arbitrary = elements [ Rotate, ScaleX, ScaleY, TranslateX, TranslateY ] <*> arbitrary

instance Arbitrary TSteps where
    arbitrary = TSteps <$> do
        (PlusMinusTwoPi a) <- arbitrary
        (NonZero x) <- arbitrary
        (NonZero y) <- arbitrary
        x' <- arbitrary
        y' <- arbitrary
        return [ Rotate a, ScaleX x, ScaleY y, TranslateX x', TranslateY y']

instance Arbitrary (Transformation V2 Double) where
    arbitrary = mkTransformation <$> arbitrary

instance Arbitrary TEllipse where
    arbitrary = do (TSteps ss) <- arbitrary
                   return $ TEllipse ss (Ellipse . mkTransformation $ ss)

instance Show TEllipse where
    show (TEllipse ss _) = "mkEllipse " ++ show ss

mkEllipse = Ellipse . mkTransformation

a .~. b = abs (a - b) < 1e-8

piAngle t
  | t < 0     = piAngle $ t + pi
  | t > pi    = piAngle $ t - pi
  | otherwise = t

angleFromTXY t sx sy = piAngle a
  where a = if abs sx > abs sy then t else t + pi/2

prop_ellipseAngleValid (TEllipse ss ell) = a .~. a'
  where ((Rotate t):(ScaleX sx):(ScaleY sy):_) = ss
        a  = ellipseAngle ell
        a' = angleFromTXY t sx sy

prop_ellipseAngleRange (TEllipse ss ell) = let a = ellipseAngle ell in a >= 0.0 && a < pi

prop_ellipseScaleOrder (TEllipse ss ell) = a >= b && a >= 0 && b >= 0
    where (a,b) = ellipseScale ell

prop_ellipseScale (TEllipse ss ell) = a .~. a' && b .~. b'
  where (_:(ScaleX sx):(ScaleY sy):_) = ss
        (x,y) = (abs sx,abs sy)
        (a,b) = if x > y then (x,y) else (y,x)
        (a',b') = ellipseScale ell

