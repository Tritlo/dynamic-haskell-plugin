{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin=Data.Dynamic.Plugin -dcore-lint #-}

module Main where

import Data.Dynamic
import Data.Dynamic.Plugin

xs :: [Dynamic]
xs = [True, 42 :: Int, False, (), "HIW"]

message :: Dynamic -> String
message = \case
  Is True -> "yes"
  Is False -> "no"
  Is () -> "unit"
  Is (x :: Int) -> "number " ++ show x
  _ -> error "Unrecognized message!"

main = do mapM_ (putStrLn . message) xs