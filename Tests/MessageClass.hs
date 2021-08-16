{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin=Data.Dynamic.Plugin -dcore-lint #-}

module Main where

import Data.Dynamic
import Data.Dynamic.Plugin

class Message a where
  message :: a -> String

instance Message Bool where
  message x = "the answer is " ++ if x then "yes" else "no"

instance Message Int where
  message x = show x ++ " is an int"

xs :: [Dynamic]
xs = [True, 42 :: Int, False, "HIW"]

main :: IO ()
main = do mapM_ (putStrLn . message) xs