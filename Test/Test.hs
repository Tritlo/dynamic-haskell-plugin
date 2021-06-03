{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fplugin=Data.Dynamic.Plugin
                -fplugin-opt=Data.Dynamic.Plugin:debug
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
