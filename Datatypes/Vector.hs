data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+1) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

plop = Vector 3 5 8 `vplus` Vector 9 2 8
blop = Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3

top = Vector 2 9 7 `vectMult` 10

slop = Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0

finalAnswer = Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)

main = putStrLn (show finalAnswer)
