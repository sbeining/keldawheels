module Data.SlotMachine
  ( SlotMachine(..)
  , Slot(..)
  , SlotItem(..)
  , spin
  , spinSlot
  )
where

import Control.Monad
import Data.Slot
import Data.SlotItem
import System.Random

data SlotMachine = SlotMachine [Slot]
  deriving Show

spin :: SlotMachine -> IO [SlotItem]
spin (SlotMachine slots) = forM slots spinSlot

spinSlot :: Slot -> IO SlotItem
spinSlot (Slot items) = do
  pos <- getStdRandom (randomR (1, length items))

  return $ items !! (pos - 1)
