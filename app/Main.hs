{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.SlotMachine

main :: IO ()
main = do
  let slot = Slot $ TextItem "Cherry" : TextItem "Lemon" : TextItem "7" : TextItem "BAR" : []
  let machine = SlotMachine $ slot : slot : slot : []

  winners <- spin machine
  print $ winners

  return ()
