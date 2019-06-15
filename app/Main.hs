{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.SlotMachine
import qualified Data.Text as T
import System.Directory
import System.Environment.Executable
import System.FilePath

main :: IO ()
main = do
  -- search in directory of the executable first and then in the current directory
  (execPath, _) <- splitExecutablePath
  exists <- doesDirectoryExist (execPath </> "slots")
  let path = if exists
      then execPath </> "slots"
      else "slots"

  slots <- buildSlots path
  let machine = SlotMachine slots

  winners <- spin machine
  print $ winners

  return ()

buildSlots :: FilePath -> IO [Slot]
buildSlots path = do
  contents <- listDirectory path

  setCurrentDirectory path
  directories <- filterM doesDirectoryExist contents
  absDirectories <- mapM makeAbsolute directories

  forM absDirectories buildSlot

buildSlot :: FilePath -> IO Slot
buildSlot path = do
  contents <- listDirectory $ path

  setCurrentDirectory path
  files <- filterM doesFileExist contents
  absFiles <- mapM makeAbsolute files

  slotItems <- forM absFiles buildSlotItem
  return $ Slot slotItems

buildSlotItem :: FilePath -> IO SlotItem
buildSlotItem path =
  case takeExtension path of
    ".txt" -> do
      content <- readFile path
      return $ TextItem <$> T.strip $ T.pack $ content
    _ -> return $ ImageItem path
