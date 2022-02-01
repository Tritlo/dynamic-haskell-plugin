{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE  ScopedTypeVariables #-}
module Main where

import Data.Dynamic
import Control.Concurrent.Chan
import Control.Concurrent

pattern Is :: forall a. (Typeable a) => a -> Dynamic
pattern Is res <- (fromDynamic @a -> Just res)

ping :: Int -> Chan Dynamic -> Chan Dynamic -> IO ()
ping 0 self other = do writeChan other (toDyn "done")
                       putStrLn "ping finished"

ping count self other =
     do writeChan other (toDyn ("ping", self))
        msg <- readChan self
        case msg of
             Is "pong" -> putStrLn "ping got pong"
             _ -> error "invalid message on channel"
        ping (count - 1) self other

pong :: Chan Dynamic -> IO ()
pong self = do msg <- readChan self
               case msg of
                 Is "done" -> putStrLn "pong finished"
                 Is ("ping", other) -> do
                   putStrLn "ping received"
                   writeChan other (toDyn "pong")
                   pong self
                 _ -> error "invalid message on channel"

main :: IO ()
main = do pingbox <- newChan
          pongbox <- newChan
          forkIO (pong pongbox)
          ping 5 pingbox pongbox