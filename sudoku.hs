import Data.List (transpose, nub)

type Value = Char
type Row a = [a]
type Matrix a = [Row a]
type Grid = Matrix Value

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes xs = map concat [(map (take 3 . drop i) . (take 3 . drop j)) xs | i <- [0, 3, 6], j <- [0, 3, 6]]

blank :: Grid
blank = replicate 9 (replicate 9 '.')

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = not (length xs == length (nub xs))

allFalse :: [Bool] -> Bool
allFalse = all (== False)

valid :: Grid -> Bool
valid xs = (allFalse (map hasDuplicates (rows xs))) && (allFalse (map hasDuplicates (cols xs))) && (allFalse (map hasDuplicates (boxes xs)))

main = do
  putStrLn "Hello, everybody!"
