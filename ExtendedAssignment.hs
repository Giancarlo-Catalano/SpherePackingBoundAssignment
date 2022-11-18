import Data.List

choose :: Integer -> Integer -> Integer
n `choose` k
  | k > n           = undefined
  | k == 0          = 1
  | k > (n `div` 2) = n `choose` (n-k)
  | otherwise       = n * ((n-1) `choose` (k-1)) `div` k
  
  
type Chance = Double  

--calculates the chance that n errors will occurr within tot bits, each having error chance chanceOfBitFlip 
chanceOfNErrors tot chanceOfBitFlip n = (chanceOfBitFlip ^ n) * ((1-chanceOfBitFlip) ^ (tot-n)) * (fromIntegral (choose tot n))

--calculates the chance of at most n errors
chanceOfAtMostNErrors tot chanceOfBitFlip n = sum $ map (chanceOfNErrors tot chanceOfBitFlip) [0 .. n]


--calculates the upper bound of M using Sphere packing
upperBoundForM :: Integer -> Integer -> Integer
upperBoundForM tot correctableErrors = (2 ^ tot) `div` (sumOfChoices)
                                            where sumOfChoices = sum $ map (choose tot) [0..correctableErrors]
                                            
         
--given a predicate, it returns the smallest non-negative integer solution         
findGreatestIntSolution :: (Integer -> Bool) -> Integer
findGreatestIntSolution predicate = fromIntegral $ length $ takeWhile predicate [0..]

-- if I have S bits, what value E such that "probability of less or equal to E errors in S" < chance
findHowManyErrorsAreWithinProbability tot chanceOfBitFlip requiredChance = findGreatestIntSolution ((< requiredChance) . (chanceOfAtMostNErrors tot chanceOfBitFlip))

--given a function f, it finds the first x such that f(x) = target
findFirstMatching :: (Integer -> Integer) -> Integer -> Integer
findFirstMatching func target = head $ dropWhile ((< target) . func) [0..]

--finds the minimum amount of bits required to satify the reliability requirement 
findMinimumSForM :: Integer -> Chance -> Chance -> Integer
findMinimumSForM m chanceOfBitFlip safety = findFirstMatching getMForGivenS m
    where getMForGivenS s = upperBoundForM s (findHowManyErrorsAreWithinProbability s chanceOfBitFlip safety)

--finds the minimum amount of bits required and the error correction ability needed for a certain reliability
findSAndTForM :: Integer -> Chance -> Chance -> (Integer, Integer)
findSAndTForM m chanceOfBitFlip safety = let s = findMinimumSForM m chanceOfBitFlip safety
                                         in  (s, findHowManyErrorsAreWithinProbability s chanceOfBitFlip safety)

--in string form, it creates a row with M, minimum S required, error correction required
showRow :: Integer -> String
showRow m = let (s, t) = findSAndTForM m 0.01 0.999999
            in  (show m) ++ "\t" ++ (show s) ++ "\t" ++ (show t)


main = putStrLn $ unlines $ map showRow [2..256]