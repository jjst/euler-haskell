{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import Data.List
import Data.Function
import Data.Char

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

isPalindrome :: (Show a) => a -> Bool
isPalindrome x = str == reverse str
    where str = show x

answer4 = maximum . filter isPalindrome $ [x*x' | x <- xs, x' <- xs]
    where xs = [100..999]

highestOccurrences :: [[Int]] -> [[Int]]
highestOccurrences xs = map (maximumBy (compare `on` length)) . groupBy ((==) `on` head) . sortBy (compare `on` head) $ xs


answer5 = product . concat . highestOccurrences . allPrimeFactors $ [2..20]
    where allPrimeFactors xs = concatMap group $ map primeFactors xs

squareOfSums xs = (sum xs)^2
sumOfSquares = sum . map (^2)

answer6 = delta [1..100]
    where delta xs = squareOfSums xs - sumOfSquares xs

answer7 = primes !! 10000

multiline :: String
multiline = [r|
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
|]

digits :: Integer -> [Int]
digits = map digitToInt . show

listsOfLength :: Int -> [Int] -> [[Int]]
listsOfLength len xs = takeWhile (\xs -> length xs == len) . map (take len) . tails $ xs

answer8 = maximum . map product $ listsOfLength 5 . digits $ number
    where number = read (concat . lines $ multiline) :: Integer
