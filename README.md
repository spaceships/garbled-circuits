garbled-circuits
================

Haskell EDSL for secure two-party computation via garbled circuits.

Two parties can make a garbled circuit to find the output of a function without
learning each other's input. The function can be any function if it can be
expressed as a boolean circuit. Each gate of the circuit encrypts its output by
using its input as keys. Since a party can only know a gate's output if it has
the right input, only the correct output is recoverable.

usage
-----

First write your function as a `Program Circ`.

>     add1Bit :: Ref Circ -> Ref Circ -> Ref Circ -> CircBuilder (Ref Circ, Ref Circ)
>     add1Bit x y c = do
>         s    <- c_xor x y
>         out  <- c_xor c s
>         cout <- bindM2 c_or (c_and x y) (c_and c s)
>         return (out, cout)
>     
>     addBits :: [Ref Circ] -> [Ref Circ] -> CircBuilder ([Ref Circ], Ref Circ)
>     addBits xs ys = do
>         f <- c_const False
>         builder xs ys f []
>       where
>         builder [] []         c outs = return (outs, c)
>         builder (x:xs) (y:ys) c outs = do
>           (out,c') <- add1Bit x y c
>           builder xs ys c' (out:outs)
>     
>     circ_NBitAdder :: Int -> Program Circ
>     circ_NBitAdder n = buildCirc $ do
>         inp1      <- replicateM n c_input
>         inp2      <- replicateM n c_input
>         (outs, _) <- addBits inp1 inp2
>         return outs
>     
>     circ_8BitAdder :: Program Circ
>     circ_8BitAdder = circ_NBitAdder 8

Then you can use `garble` to transform it into a `Program GarbledGate`.

>     -- convert to GarbledGate and use GG evaluator
>     eval_8BitAdderGG :: Word8 -> Word8 -> IO Word8
>     eval_8BitAdderGG x y = do
>         gg <- garble circ_8BitAdder
>         result <- evalGG (word2Bits x ++ word2Bits y) gg
>         return (bits2Word result)

Now we can add `Word8`s obliviously!

>     ghci> eval_8BitAdderGG 21 21
>     42

architecture
------------

garbled-circuits consists of three languages: `Circ`, `TruthTable`, and `GarbledGate`.

`Circ` is for building circuits. It's the user-facing language. It's available
in `Garbled.Circuits.Language`. It has smart constructors like `c_or` and
`c_not` as in the example above.

`TruthTable` is an intermediate langauge between `Circ` and `GarbledGate`. It's
role is to compress gates that are possibly unary or nullary (like `not` and
`const`) into the binary gates above them. The user shouldn't have to worry
about TruthTable.

`GarbledGate` is a garbled circuit. Each binary gate in `TruthTable` gets
assigned a true and a false wire label. This wirelabel is the input
to the gates above in the circuit. Each gate encrypts its output with the
correct input as keys.

License
-------

garbled-circuits is licenced under Apache 2.0

Copyright 2015 Brent Carmer
