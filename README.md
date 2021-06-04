The Data.Dynamic Plugin
======

The Data.Dynamic Plugin adds extra features for the Dynamic type, such as
allowing GHC to automatically marshal to and from Dynamic (by wrapping variables
in `fromDyn` or `castDyn` repsectively) and by allowing for dynamic dispatch on
Dynamic types by generating dispatch for the various typeclass instances as
required.

The code compiles and runs, with the correct core being generated (as verified
by `-dcore-lint`).

Example compiling and running `cabal run Test/` the following in the current
directory

```haskell
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fplugin=Data.Dynamic.Plugin
                -dcore-lint
                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Dynamic
import Data.Dynamic.Plugin

data A = A | B deriving (Show)
data C = C deriving (Show)

class Foo a where
   foo :: a -> Int
   insts :: Show a => a -> String

instance Foo A where
    foo _ = 10
    insts x = "A: " ++ show x

instance Foo C where
    foo _ = 20
    insts x = "C: " ++ show x

isWithDyn :: IO ()
isWithDyn = print $ case toDyn A of
             Is A -> "was 1A"
             Is B -> "was 1B"
             Is C -> "was 1C"
             _ -> undefined

isDirect ::  IO ()
isDirect = print $ case C of
             Is A -> "was 2A"
             Is B -> "was 2B"
             Is C -> "was 2C"
             _ -> undefined

main :: IO ()
main = do

  let s = [A,B,C] :: [Dynamic]
  mapM print s
  mapM_ (print . foo) s
  mapM_ (print . insts) s
  let a = A :: Dynamic
  print (a :: A)

  isWithDyn
  isDirect
```

will yield:

```console
Build profile: -w ghc-8.10.4 -O1
In order, the following will be built (use -v for more details):
 - Test-1.0.0 (exe:test_dyn) (file Test.hs changed)
Preprocessing executable 'test_dyn' for Test-1.0.0..
Building executable 'test_dyn' for Test-1.0.0..
[1 of 1] Compiling Main             ( Test.hs, /home/tritlo/dynamic-haskell-plugin/dist-newstyle/build/x86_64-linux/ghc-8.10.4/Test-1.0.0/x/test_dyn/build/test_dyn/test_dyn-tmp/Main.o )

Test.hs:40:14: warning: Marshalling ‘C’ to Dynamic
   |
40 |              Is A -> "was 2A"
   |              ^^^^

Test.hs:41:14: warning: Marshalling ‘C’ to Dynamic
   |
41 |              Is B -> "was 2B"
   |              ^^^^

Test.hs:42:14: warning: Marshalling ‘C’ to Dynamic
   |
42 |              Is C -> "was 2C"
   |              ^^^^

Test.hs:48:12: warning: Marshalling ‘A’ to Dynamic
   |
48 |   let s = [A,B,C] :: [Dynamic]
   |            ^

Test.hs:48:14: warning: Marshalling ‘A’ to Dynamic
   |
48 |   let s = [A,B,C] :: [Dynamic]
   |              ^

Test.hs:48:16: warning: Marshalling ‘C’ to Dynamic
   |
48 |   let s = [A,B,C] :: [Dynamic]
   |                ^

Test.hs:50:18: warning:
    Building dispatch table for ‘Foo Dynamic’ based on
    ‘instance Foo A -- Defined at Test.hs:23:10’
    ‘instance Foo C -- Defined at Test.hs:27:10’
   |
50 |   mapM_ (print . foo) s
   |                  ^^^

Linking /home/tritlo/dynamic-haskell-plugin/dist-newstyle/build/x86_64-linux/ghc-8.10.4/Test-1.0.0/x/test_dyn/build/test_dyn/test_dyn ...
<<A>>
<<A>>
<<C>>
10
10
20
"A: A"
"A: B"
"C: C"
"was 1A"
"was 2C"
```
Build profile: -w ghc-8.10.4 -O1
In order, the following will be built (use -v for more details):
 - dynamic-haskell-plugin-0.0.1 (lib) (file Data/Dynamic/Plugin.hs changed)
 - Test-1.0.0 (exe:test_dyn) (dependency rebuilt)
Preprocessing library for dynamic-haskell-plugin-0.0.1..
Building library for dynamic-haskell-plugin-0.0.1..
[1 of 1] Compiling Data.Dynamic.Plugin ( Data/Dynamic/Plugin.hs, /home/tritlo/dynamic-haskell-plugin/dist-newstyle/build/x86_64-linux/ghc-8.10.4/dynamic-haskell-plugin-0.0.1/build/Data/Dynamic/Plugin.o, /home/tritlo/dynamic-haskell-plugin/dist-newstyle/build/x86_64-linux/ghc-8.10.4/dynamic-haskell-plugin-0.0.1/build/Data/Dynamic/Plugin.dyn_o )
