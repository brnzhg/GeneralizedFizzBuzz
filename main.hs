import Data.List (intercalate, cycle)
import Data.Maybe (maybe, catMaybes)

data GFizzBuzz = Coprime Int | FactorLabels [String]

instance Show (GFizzBuzz) where
  show (Coprime i) = show i
  show (FactorLabels labels) = concat labels
  

intToFactorLabels :: [(Int, String)] -> Int -> [String]
intToFactorLabels = flip f
  where  
    f i = let getLabel (factor, label)
                | i `mod` factor == 0 = Just label
                | otherwise = Nothing
          in catMaybes . map getLabel
      
gFizzBuzzSequence :: [(Int, String)] -> [GFizzBuzz]
gFizzBuzzSequence labelsByFactor = 
    zipWith f labelsSequence [1..] 
  where
    f [] = Coprime
    f labels = const $ FactorLabels labels
    labelsSequence = cycle $ intToFactorLabels labelsByFactor <$> [1..n]
    n = foldr lcm 1 $ fst <$> labelsByFactor

main = do
  let printGFizzBuzzList = putStrLn . intercalate "," . fmap show
      classicRules = [(3, "Fizz"), (5, "Buzz")]
      snapCracklePopRules = [(2, "Snap"), (3, "Crackle"), (5, "Pop")]
  printGFizzBuzzList . take 20 $ gFizzBuzzSequence classicRules
  printGFizzBuzzList . take 20 $ gFizzBuzzSequence snapCracklePopRules
