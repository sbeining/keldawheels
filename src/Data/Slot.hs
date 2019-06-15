module Data.Slot
  ( Slot(..)
  , fromFilePath
  )
where

import Control.Monad
import qualified Data.SlotItem as SlotItem
import System.Directory

data Slot = Slot [SlotItem.SlotItem]
  deriving Show

fromFilePath :: FilePath -> IO Slot
fromFilePath path = do
  contents <- listDirectory $ path

  setCurrentDirectory path
  files <- filterM doesFileExist contents
  absFiles <- mapM makeAbsolute files

  slotItems <- forM absFiles SlotItem.fromFilePath
  return $ Slot slotItems

