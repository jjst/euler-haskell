import Data.List
import Data.Function
import Data.Char
import Data.Maybe
import Data.Array
import Inputs

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

primes :: (Integral a) => [a]
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

primeFactors :: (Integral a) => a -> [a]
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

digits :: Integer -> [Int]
digits = map digitToInt . show

unDigits :: [Int] -> Integer
unDigits = read . map intToDigit

listsOfLength :: Int -> [a] -> [[a]]
listsOfLength len xs = takeWhile (\xs -> length xs == len) . map (take len) . tails $ xs

answer8 = maximum . map product $ listsOfLength 5 . digits $ number
    where number = read (concat . lines . fromJust $ (input 8) ) :: Integer

answer9 = product . head $ [ [a,b,c] | b <- [1..], a <- [1..b-1], let s = 1000, let c = (s - a - b), a^2 + b^2 == c^2 ]

-- Not very efficient, runs in ~10s locally
answer10 = sum . takeWhile (< 2000000) $ primes

lstrip :: String -> String
lstrip = dropWhile isSpace
rstrip :: String -> String
rstrip = reverse . lstrip . reverse
strip :: String -> String
strip = lstrip . rstrip

grid = map (map read . words) . lines . strip . fromJust $ (input 11) :: [[Int]]

diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal (xs:xxs) = head xs : diagonal (map tail xxs)

bottom = tail . map init
top = init . map tail

bottoms :: [[a]] -> [[[a]]]
bottoms [] = []
bottoms xxs = b : bottoms b
    where b = bottom xxs

tops :: [[a]] -> [[[a]]]
tops [] = []
tops xxs = t : tops t
    where t = top xxs

allDiagonals :: [[a]] -> [[a]]
allDiagonals m = diags m ++ diags (map reverse m)
    where diags m = map diagonal $ m : (bottoms m ++ tops m)

answer11 = maximum . map product $ all4InRows
    where all4InRows = concatMap (listsOfLength 4) $ grid ++ transpose grid ++ allDiagonals grid

triangleNumbers = 1 : zipWith (+) triangleNumbers [2..]

divisors' :: (Integral a) =>  a -> [a]
divisors' x = reverse $ x : foldl (\acc d -> if x `mod` d == 0 then d : acc else acc) [] [1..(x `div` 2)]

numberOfDivisors :: (Integral a) => a -> Int
numberOfDivisors 1 = 1
numberOfDivisors x = product . map (\xs -> length xs + 1) . group . primeFactors $ x

answer12 = fromJust . find (\x -> numberOfDivisors x > 500) $ triangleNumbers

answer13 = unDigits . take 10 . digits . sum $ numbers
    where numbers = map read . lines . strip . fromJust $ (input 13) :: [Integer]

collatzSequence :: Integer -> [Integer]
collatzSequence 1 = [1]
collatzSequence n
    | even n    = n : collatzSequence (n `div` 2)
    | otherwise = n : collatzSequence (3*n + 1)

answer14 = maximumBy (compare `on` (length . collatzSequence)) [1..1000000]

nbLatticePaths :: Integer -> Integer
nbLatticePaths gridSize = binomialCoeffs ! (x,x)
    where binomialCoeffs =  array ((1,1),(x,x)) [ ((i,j), if (i == 1 || j == 1) then 1 else binomialCoeffs!(i-1,j) + binomialCoeffs!(i,j-1) ) | i <- [1..x], j <- [1..x]]
          x = gridSize + 1

answer15 = nbLatticePaths 20

answer16 = sum . digits $ 2^1000
