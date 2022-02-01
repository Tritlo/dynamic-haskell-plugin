{-# OPTIONS_GHC -fplugin=Data.Dynamic.Plugin -dcore-lint #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Dynamic
import Data.Dynamic.Plugin
import Control.Concurrent.Chan
import Control.Concurrent

type Mailbox = Chan Dynamic

ping :: Int -> Mailbox -> Mailbox -> IO ()
ping 0 self other = do writeChan other "done"
                       putStrLn "ping finished"

ping count self other =
     do writeChan other ("ping", self)
        msg <- readChan self
        case msg of
          "pong" -> putStrLn "ping got pong"
          _ -> error "invalid message on channel"
        ping (count - 1) self other

pong :: Mailbox -> IO ()
pong self = do msg <- readChan self
               case msg of
                Is "done" -> putStrLn "pong finished"
                Is ("ping", other :: Mailbox) -> do
                  putStrLn "ping received"
                  writeChan other "pong"
                  pong self
                _ -> error "invalid message on channel"

main :: IO ()
main = do pingbox <- newChan
          pongbox <- newChan
          forkIO (pong pongbox)
          ping 5 pingbox pongbox


