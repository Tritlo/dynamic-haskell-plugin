{-# OPTIONS_GHC -fplugin=Data.Dynamic.Plugin -dcore-lint #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, RankNTypes #-}
module Main where

import Data.Dynamic
import Data.Dynamic.Plugin
import Control.Concurrent.Chan
import Control.Concurrent

type Mailbox = Chan Dynamic

class Printable a where
  printIt :: a -> String

instance Printable (Int, Int) where
  printIt = show

instance Printable String where
  printIt = id

client :: Int -> Mailbox -> Mailbox -> IO ()
client 0 self other = do writeChan other "done"
                         putStrLn "client finished"

client count self other =
     do writeChan other ("ping", self)
        msg <- readChan self
        case msg of
          "pong" -> do
            putStrLn "printing!"
            writeChan other ("print", (count, count) :: Dynamic)
            writeChan other ("print", "thanks" :: Dynamic)
          _ -> error "printer dead!"
        client (count - 1) self other

printerService :: Mailbox -> IO ()
printerService self =
  do msg <- readChan self
     case msg of
      Is "done" -> putStrLn "pong finished"
      Is ("ping", other :: Mailbox) -> do
        putStrLn "ping received"
        writeChan other "pong"
        printerService self
      Is ("print", toPrint :: Dynamic) -> do
        putStrLn "print request received"
        putStrLn (printIt toPrint)
        printerService self
      _ -> error "invalid message on channel"

main :: IO ()
main = do printbox <- newChan
          forkIO (printerService printbox)
          clientbox <- newChan
          client 5 clientbox printbox
