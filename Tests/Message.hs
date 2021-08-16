{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin=Data.Dynamic.Plugin
                -dcore-lint #-}

module Main where

import Data.Dynamic
import Data.Dynamic.Plugin

xs :: [Dynamic]
xs = [True, 42 :: Int, False, "HIW"]

message :: Dynamic -> String
message = \case
  Is (x :: Bool) ->
    "the answer is " ++ if x then "yes" else "no"
  Is (x :: Int) ->
    show x ++ " is an int"
  _ -> error "Unrecognized message!"

main = do mapM_ (putStrLn . message) xs