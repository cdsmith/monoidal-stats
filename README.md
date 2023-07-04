monoidal-stats: Statistics that accumulate as a monoid

Calculate populations, means, variances, and covariances in an arbitrary vector
space in a single pass, in parallel, or incrementally.  Generalizing Welford's
algorithm, this library provides a monoidal interface for calculating statistics
that accumulate.

## Example

```haskell
import Data.MonoidalStats

dataSet :: [Vector Double]
dataSet = ...

stats :: Covariance Vector Double
stats = foldMap observation dataSet

myPopulation :: Double
myPopulation = population stats

myMean :: Vector Double
myMean = mean stats

myVariance :: Double
myVariance = populationVariance stats

myDimensionVariances :: Vector Double
myDimensionVariances = populationVariances stats

myCovarianceMatrix :: Vector (Vector Double)
myCovarianceMatrix = populationCovariance stats
```

## TODO

* Consider Kahan summation for better numerical stability.  See Data.Kahan for a
  start.
