{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fplugin=Data.Dynamic.Plugin
                -dcore-lint
                 #-}
                --fplugin-opt=Data.Dynamic.Plugin:debug
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Dynamic
import Data.Dynamic.Plugin
import Data.List

data A = A | B deriving (Show, Eq, Ord)
data C = C deriving (Show, Eq, Ord)

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

  isWithDyn
  isDirect
  -- Fails, since we don't create an instance of Eq [C], since we cannot
  -- construct that when we don't know what [C] is!
  print $ ([A] :: Dynamic) == ([C] :: Dynamic)
    
  -- Note: Does not typecheck! Which is good, because how would we sort
  -- based on the different types?
  -- pr nt ((A :: Dynamic) <= (C :: Dynamic))
  -- Typechecks and works
  print ( (max (A :: Dynamic) (B :: Dynamic)) :: A )
  print ( (max (B :: Dynamic) (A :: Dynamic)) :: A)
  -- Typechecks, but returns a Dynamic
  print ( (max (A :: Dynamic) (C :: Dynamic)) )
  
  -- All of these typecheck, but return a runtime error.
  -- print ( (max (C :: Dynamic) (A :: Dynamic)) :: A ) -- Does not typecheck, good!
  -- print ( (max (C :: Dynamic) (A :: Dynamic)) :: C ) -- Doesn't typecheck either, great!
  -- print ( (max (C :: Dynamic) (B :: Dynamic)) :: A )
  -- print ( (max (C :: Dynamic) (B :: Dynamic)) :: C )
  -- print ( (max (A :: Dynamic) (C :: Dynamic)) :: A)
  -- print ( (max (A :: Dynamic) (C :: Dynamic)) :: C)
  -- print ( (max (B :: Dynamic) (C :: Dynamic)) :: A)
  -- print ( (max (B :: Dynamic) (C :: Dynamic)) :: C)
  -- Returns a type error, but it's located on the first use of the dictionarier,
  -- because that's where the error (i.e. the instance) originates from!
  -- print $ sort s

