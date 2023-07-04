module Data.Kahan
  ( KahanSum,
    toKahanSum,
    fromKahanSum,
  )
where

data KahanSum a = KahanSum a a

toKahanSum :: Num a => a -> KahanSum a
toKahanSum x = KahanSum x 0

fromKahanSum :: Num a => KahanSum a -> a
fromKahanSum (KahanSum big little) = big + little

instance (Ord a, Num a) => Num (KahanSum a) where
  fromInteger n = KahanSum (fromInteger n) 0
  negate (KahanSum big little) = KahanSum (-big) (-little)
  abs (KahanSum big little)
    | abs big < abs little = abs (KahanSum little big)
    | big < 0 = KahanSum (-big) (-little)
    | otherwise = KahanSum big little
  signum (KahanSum big little) = KahanSum (signum (big + little)) 0
  KahanSum bigX littleX + KahanSum bigY littleY = KahanSum bigZ littleZ
    where
      bigZ
        | abs bigX > abs bigY = bigX + (bigY + (littleX + littleY))
        | otherwise = bigY + (bigX + (littleX + littleY))
      err
        | abs bigX > abs bigY = (bigX - bigZ) + bigY
        | otherwise = (bigY - bigZ) + bigX
      littleZ = err + littleX + littleY
  KahanSum bigX littleX * KahanSum bigY littleY
    | abs bigX > abs bigY = KahanSum (a + c) (b + d)
    | otherwise = KahanSum (a + b) (c + d)
    where
      a = bigX * bigY
      b = littleX * bigY
      c = bigX * littleY
      d = littleX * littleY
