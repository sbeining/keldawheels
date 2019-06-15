{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.SlotMachine as SlotMachine
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

  machine <- SlotMachine.fromFilePath path

  winners <- SlotMachine.spin machine
  print $ winners
