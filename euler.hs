import Data.List
import Data.Function

answer1 = sum $ filter (\x -> x `mod` 5 == 0 || x `mod` 3 == 0) [1..999]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

answer2 = sum $ filter even $ takeWhile (< 4000000) fibs

minus :: Ord a => [a] -> [a] -> [a]
minus [] _ = []
minus xs [] = xs
minus l1@(x:xs) l2@(y:ys)
    | x > y = minus l1 ys
    | x < y = x : minus xs l2
    | otherwise = minus xs l2

primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

-- inefficient prime number generation (Turner's sieve)
primesT = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, rem x p /= 0]

primeFactors n | n > 1 = go n primes
   where
     go n ps@(p:ps')
        | p*p > n        = [n]
        | n `rem` p == 0 =  p : go (n `quot` p) ps
        | otherwise      =      go n ps'

answer3 = maximum . primeFactors $ 600851475143

highestOccurrences :: [[Int]] -> [[Int]]
highestOccurrences xs = map (maximumBy (compare `on` length)) . groupBy ((==) `on` head) . sortBy (compare `on` head) $ xs


answer5 = product . concat . highestOccurrences . allPrimeFactors $ [2..20]
    where allPrimeFactors xs = concatMap group $ map primeFactors xs

squareOfSums xs = (sum xs)^2
sumOfSquares = sum . map (^2)

answer6 = delta [1..100]
    where delta xs = squareOfSums xs - sumOfSquares xs
