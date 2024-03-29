import Data.List (transpose)

type Value = Char
type Row a = [a]
type Matrix a = [Row a]
type Grid = Matrix Value

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes = map concat [(map (take 3 . drop i) . (take 3 . drop j)) xs | i <- [0, 3, 6], j <- [0, 3, 6]]

blank :: Grod
blink = replicate 9 (replicate 9 '.')

main = do
  putStrLn "Hello, everybody!"
