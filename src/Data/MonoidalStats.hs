{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.MonoidalStats where

import Data.Foldable (foldl')
import Linear
  ( Additive,
    Metric,
    Trace,
    diagonal,
    outer,
    quadrance,
    zero,
    (!!/),
    (!+!),
    (*!!),
    (*^),
    (^+^),
    (^-^),
    (^/),
  )

class (Monoid (t f a), Num a) => Observable t f a where
  observation :: f a -> t f a
  default observation :: (Divisible t f a) => f a -> t f a
  observation = weightedObservation 1

  population :: t f a -> a

class (Observable t f a, Fractional a) => HasMean t f a where
  total :: t f a -> f a

  mean :: t f a -> f a
  default mean :: (Additive f, Eq a, Fractional a) => t f a -> f a
  mean x
    | population x == 0 = zero
    | otherwise = total x ^/ population x

class (HasMean t f a) => HasVariance t f a where
  sumSquaredDistance :: t f a -> a
  default sumSquaredDistance :: (Foldable f, HasVariances t f a) => t f a -> a
  sumSquaredDistance = sum . sumSquaredDifference

  populationVariance :: t f a -> a
  populationVariance x = sumSquaredDistance x / population x

  sampleVariance :: t f a -> a
  sampleVariance x = sumSquaredDistance x / (population x - 1)

class (HasVariance t f a, Functor f) => HasVariances t f a where
  sumSquaredDifference :: t f a -> f a
  default sumSquaredDifference :: (HasCovariance t f a, Trace f) => t f a -> f a
  sumSquaredDifference = diagonal . scatterMatrix

  populationVariances :: t f a -> f a
  populationVariances x = sumSquaredDifference x ^/ population x

  sampleVariances :: t f a -> f a
  sampleVariances x = sumSquaredDifference x ^/ (population x - 1)

class (HasVariances t f a) => HasCovariance t f a where
  scatterMatrix :: t f a -> f (f a)

  populationCovariance :: t f a -> f (f a)
  populationCovariance x = scatterMatrix x !!/ population x

  sampleCovariance :: t f a -> f (f a)
  sampleCovariance x = scatterMatrix x !!/ (population x - 1)

class (Observable t f a, Fractional a) => Divisible t f a where
  weightedObservation :: a -> f a -> t f a
  scalePopulation :: a -> t f a -> t f a

newtype Count f a = Count a
  deriving (Eq, Ord, Num, Show)

instance (Num a) => Semigroup (Count f a) where
  Count a <> Count b = Count (a + b)

instance (Num a) => Monoid (Count f a) where
  mempty = Count 0

instance (Num a) => Observable Count f a where
  observation _ = Count 1
  population (Count n) = n

instance (Fractional a) => Divisible Count f a where
  weightedObservation w _ = Count w
  scalePopulation scale (Count n) = Count (scale * n)

data Mean f a = Mean !(Count f a) !(f a)
  deriving (Eq, Ord, Show)

instance (Additive f, Fractional a) => Semigroup (Mean f a) where
  Mean n1 t1 <> Mean n2 t2 = Mean (n1 <> n2) (t1 ^+^ t2)

instance (Additive f, Fractional a) => Monoid (Mean f a) where
  mempty = Mean mempty zero

instance (Additive f, Fractional a) => Observable Mean f a where
  population (Mean n _) = population n

instance (Additive f, Eq a, Fractional a) => HasMean Mean f a where
  total (Mean _ t) = t
  mean (Mean n t)
    | population n == 0 = zero
    | otherwise = t ^/ population n

instance (Additive f, Fractional a) => Divisible Mean f a where
  weightedObservation w v = Mean (weightedObservation w v) (w *^ v)
  scalePopulation scale (Mean n t) =
    Mean (scalePopulation scale n) (scale *^ t)

data Variance f a = Variance !(Mean f a) !a
  deriving (Eq, Ord, Show)

instance (Metric f, Eq a, Fractional a) => Semigroup (Variance f a) where
  Variance m1 s1 <> Variance m2 s2
    | population m1 * population m2 /= 0 = Variance m s
    | otherwise = Variance m (s1 + s2)
    where
      m = m1 <> m2
      s =
        (s1 + s2)
          + quadrance (population m1 *^ total m2 ^-^ population m2 *^ total m1)
            / (population m1 * population m2 * population m)

instance (Metric f, Eq a, Fractional a) => Monoid (Variance f a) where
  mempty = Variance mempty 0

instance (Metric f, Eq a, Fractional a) => Observable Variance f a where
  population (Variance m _) = population m

instance (Metric f, Eq a, Fractional a) => HasMean Variance f a where
  total (Variance m _) = total m
  mean (Variance m _) = mean m

instance (Metric f, Eq a, Fractional a) => HasVariance Variance f a where
  sumSquaredDistance (Variance _ s) = s
  populationVariance (Variance m s) = s / population m
  sampleVariance (Variance m s) = s / (population m - 1)

instance (Metric f, Eq a, Fractional a) => Divisible Variance f a where
  weightedObservation w v = Variance (weightedObservation w v) 0
  scalePopulation scale (Variance m s) =
    Variance (scalePopulation scale m) (scale * s)

data Covariance f a = Covariance !(Mean f a) !(f (f a))

deriving instance (Eq a, Eq (f a), Eq (f (f a))) => Eq (Covariance f a)

deriving instance
  (Eq a, Ord a, Eq (f a), Ord (f a), Eq (f (f a)), Ord (f (f a))) =>
  Ord (Covariance f a)

deriving instance (Show a, Show (f a), Show (f (f a))) => Show (Covariance f a)

instance (Additive f, Eq a, Fractional a) => Semigroup (Covariance f a) where
  Covariance m1 s1 <> Covariance m2 s2
    | population m1 * population m2 /= 0 = Covariance m s
    | otherwise = Covariance m (s1 !+! s2)
    where
      m = m1 <> m2
      delta = population m1 *^ total m2 ^-^ population m2 *^ total m1
      s =
        (s1 !+! s2)
          !+! (delta `outer` delta)
          !!/ (population m1 * population m2 * population m)

instance
  (Applicative f, Additive f, Eq a, Fractional a) =>
  Monoid (Covariance f a)
  where
  mempty = Covariance mempty (pure zero)

instance
  (Applicative f, Additive f, Eq a, Fractional a) =>
  Observable Covariance f a
  where
  population (Covariance m _) = population m

instance
  (Applicative f, Additive f, Eq a, Fractional a) =>
  HasMean Covariance f a
  where
  total (Covariance m _) = total m
  mean (Covariance m _) = mean m

instance
  (Foldable f, Applicative f, Additive f, Trace f, Eq a, Fractional a) =>
  HasVariance Covariance f a

instance
  (Foldable f, Applicative f, Additive f, Trace f, Eq a, Fractional a) =>
  HasVariances Covariance f a

instance
  (Foldable f, Applicative f, Additive f, Trace f, Eq a, Fractional a) =>
  HasCovariance Covariance f a
  where
  scatterMatrix (Covariance _ s) = s

instance
  (Applicative f, Additive f, Eq a, Fractional a) =>
  Divisible Covariance f a
  where
  weightedObservation w v = Covariance (weightedObservation w v) (pure zero)
  scalePopulation scale (Covariance m s) =
    Covariance (scalePopulation scale m) (scale *!! s)

newtype Members t f a = Members (t (f a))
  deriving (Eq, Ord, Show, Functor, Foldable, Monoid, Semigroup)

instance
  (Foldable t, Applicative t, Monoid (t (f a)), Num a) =>
  Observable (Members t) f a
  where
  observation v = Members (pure v)
  population (Members ms) = fromIntegral (length ms)

instance
  ( Foldable t,
    Applicative t,
    Monoid (t (f a)),
    Additive f,
    Eq a,
    Fractional a
  ) =>
  HasMean (Members t) f a
  where
  total (Members ms) = foldl' (^+^) zero ms

instance
  ( Foldable t,
    Applicative t,
    Monoid (t (f a)),
    Additive f,
    Eq a,
    Fractional a,
    Metric f
  ) =>
  HasVariance (Members t) f a
  where
  sumSquaredDistance x@(Members ms) = sum (quadrance . (^-^ mean x) <$> ms)

instance
  ( Foldable t,
    Applicative t,
    Monoid (t (f a)),
    Additive f,
    Eq a,
    Fractional a,
    Metric f
  ) =>
  HasVariances (Members t) f a
  where
  sumSquaredDifference x@(Members ms) =
    foldl' (^+^) zero (fmap (^ (2 :: Int)) . (^-^ mean x) <$> ms)
