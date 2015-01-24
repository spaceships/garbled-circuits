module Garbled.Circuits where

-- circuit language
-- garbled circuit representation -- needs to be serializable
-- garbling
-- ot
-- evaluation

-- doesn't capture sharing
data Circ = Input Int
          | Output Int Circ
          | Const Bool
          | Xor Circ Circ
          | And Circ Circ
          | Or  Circ Circ
          | Not Circ Circ
          deriving (Show)

add1Bit :: Circ -> Circ -> Circ -> (Circ, Circ)
add1Bit x y c = (out, cout)
  where
    s    = Xor x y
    out  = Xor c s
    cout = Or (And x y) (And c s)

addBits :: [Circ] -> [Circ] -> ([Circ], Circ)
addBits xs ys = builder xs ys (Const False) []
  where
    builder [] []         c outs = (outs, c)
    builder (x:xs) (y:ys) c outs = builder xs ys c' (out:outs)
      where (out,c') = add1Bit x y c


circ_8BitAdder :: [Circ]
circ_8BitAdder = zipWith Output [0..7] outs
  where
    inp1 = map Input [0..7]
    inp2 = map Input [8..15]
    (outs, _) = addBits inp1 inp2

