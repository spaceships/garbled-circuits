![garbled circuit picture](https://web.engr.oregonstate.edu/~rosulekm/scbib.png)
garbled-circuits
================

This library is a Haskell DSL for secure two-party computation with garbled
circuits. You can use it to create secure protocols in the honest but curious
model. That is - parties in the protocols won't find out each other's inputs, as
long as they follow the protocol.

overview
========

Two parties can make a garbled circuit to find the output of a function without
learning each other's input. The function can be any that can be expressed as a
boolean circuit. Each gate of the circuit encrypts its output by using its input
as keys. Since a party can only know a gate's output if it has the right input,
only the correct output is recoverable.

The two parties are the `Garbler` and the `Evaluator`, who agree on a circuit to
compute. The `Garbler` creates random values for each wire in the circuit (we
call them wirelabels) and encrypts the output of each gate with its input
wirelabels. The `Garbler` begins the protocol by sending its input wirelabels
and the garbled circuit to the `Evaluator`. Next, the `Evaluator` needs to find
out what its input wirelabels are, but do so without revealing what its input
is. We use [oblivious transfer](https://en.wikipedia.org/wiki/Oblivious_transfer) 
for that. The `Evaluator` evaluates the circuit and sends the output wirelabels back to the `Garbler`, who ungarbles them and reveals the output to the `Evaluator`.

`garbled-circuits` supports the latest circuit-size optimizations, including
[half-gates](http://eprint.iacr.org/2014/756), and uses AES128 with AESNI
support for hashing. It uses [oblivious transfer
extension](https://web.engr.oregonstate.edu/~rosulekm/scbib/index.php?n=Paper.IKNP03)
to minimize the number of expensive oblivious transfers (which are based on
asymmetric crypto).

usage
-----

In this section, we'll show how to construct a simple bitwise 'and' protocol.
We'll be constructing two source files: `Garbler.hs` and `Evaluator.hs` to
be the respecive parties in the protocol. You can find these files in the 
`examples/bit-and` directory.

First we define a circuit using the `Crypto.GarbledCircuits.Language` module.
We use the smart constructors in the way you'd expect any normal monadic code
to work. Note, since this is a two-party protocol, we need to specify where
input comes from. That's what the `Garbler` and `Evaluator` data constructors
are for.

The following code is common to both `Garbler.hs` and `Evaluator.hs`

```haskell
module Main where

import Prelude hiding (and)
import System.Environment

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Language

bitAnd :: Program Circuit
bitAnd = buildCircuit $ do
    x <- input Garbler
    y <- input Evaluator
    z <- and x y
    return [z]
```

Next, we define a simple `main` function for `Garbler.hs`.  The `*Proto`
functions last argument is a pair that provide sending and recieving
`ByteString`s. `simpleConn` makes such a pair out of a `Handle`, which is
provided by `listenAt` and `connectTo`. See `Crypto.GarbledCircuits.Network` for
more details.

```haskell
main :: IO ()
main = do
    args <- getArgs
    let port  = read (args !! 0)
        inp   = read (args !! 1) :: Bool
        proto = garblerProto bitAnd [inp] . simpleConn
    result <- listenAt port proto
    print result
```

We do the same for `Evaluator.hs`.

```haskell
main :: IO ()
main = do
    args <- getArgs
    let server = args !! 0
        port   = read (args !! 1)
        inp    = read (args !! 2) :: Bool
        proto  = evaluatorProto bitAnd [inp] . simpleConn
    result <- connectTo server port proto
    print result
```

Finally, once we compile, we can preform an oblivious bitwise and! The final
line is the output.

```shell
% ./bit-and-garbler.a 12345 True
[garblerProto] circuit garbled
[garblerProto] sending circuit
[garblerProto] sending my input wires
[garblerProto] sending key
[garblerProto] performing OT
[garblerProto] recieving output
[garblerProto] sending ungarbled output
[False]
```

```shell
% ./bit-and-evaluator.a localhost 12345 False
[evaluatorProto] recieving circuit
[evaluatorProto] recieving garbler input wires
[evaluatorProto] recieving key
[evaluatorProto] performing OT
[evaluatorProto] evaluating garbled circuit
<0>: in0 Garbler wl1 2c7ec10d3db507bd48357360650fa319
<1>: in1 Evaluator 
<2>: HALFGATE <0> <1> out0
  wl0 1a164c333d7ab142e8388cd940f55144
  wl1 854e5157edd04dcbe7779dfba2a02151
[eval] <2>[0,1] HalfGate result = wl0 23f6a3ac3c66315fd9915672ac1b5022
[evaluatorProto] output =
  <2> wl0 23f6a3ac3c66315fd9915672ac1b5022
[evaluatorProto] sending output wires
[evaluatorProto] recieving ungarbled output
[False]
```

architecture
------------

`garbled-circuits` consists of three languages: `Circuit`, `TruthTable`, and
`GarbledGate`.

`Circuit` is for building boolean circuits. It's the user-facing language. It's
available in `Garbled.Circuits.Language`. It has smart constructors like `or`
and `not` as in the example above. You can use the smart constructor to get a
reference to the output, which you use as an argument to other constructors.
The `CircuitBuilder` monad compiles down to a boolean circuit.

`TruthTable` is an intermediate langauge between `Circuit` and `GarbledGate`.
It's role is to compress gates that are possibly unary or nullary (like `not`
and `const`) into the binary gates above them. The user shouldn't have to worry
about TruthTable.

`GarbledGate` is a garbled circuit. Each binary gate in `TruthTable` gets
assigned a true and a false wire label. This wirelabel is the input
to the gates above in the circuit. Each gate encrypts its output with the
correct input as keys. The `Evaluator` is only able to decrypt the output
corresponding to the input wires it holds, ensuring authentic output of the
whole garbled circuit.

todo list
---------

* circuit language - done
* intermediate TruthTable representation - done
* garbling
    * AES hashing - done
    * crypto rng - done
    * garbling optimizations (free xor, half gates) - done
* network boilerplate - done
* oblivious transfer - in progress
    * find more appropriate OT - in progress
    * OT extension - done
* haddock documentation - in progress
* profiling

license
-------

garbled-circuits is licenced under Apache 2.0

EXPERIMENTAL: USE AT YOUR OWN RISK

Copyright 2015 Brent Carmer
