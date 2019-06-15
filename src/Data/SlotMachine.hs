module Data.SlotMachine
  ( SlotMachine(..)
  , Slot.Slot(..)
  , SlotItem.SlotItem(..)
  , spin
  , spinSlot
  , fromFilePath
  )
where

import Control.Monad
import qualified Data.Slot as Slot
import qualified Data.SlotItem as SlotItem
import System.Directory
import System.Random

data SlotMachine = SlotMachine [Slot.Slot]
  deriving Show

spin :: SlotMachine -> IO [SlotItem.SlotItem]
spin (SlotMachine slots) = forM slots spinSlot

spinSlot :: Slot.Slot -> IO SlotItem.SlotItem
spinSlot (Slot.Slot items) = do
  pos <- getStdRandom (randomR (1, length items))

  return $ items !! (pos - 1)

fromFilePath :: FilePath -> IO SlotMachine
fromFilePath path = do
  contents <- listDirectory path

  setCurrentDirectory path
  directories <- filterM doesDirectoryExist contents
  absDirectories <- mapM makeAbsolute directories

  slots <- forM absDirectories Slot.fromFilePath
  return $ SlotMachine slots
