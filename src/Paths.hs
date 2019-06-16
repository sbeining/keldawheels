module Paths
  ( getSlotsPath
  )
where

import System.Directory (doesDirectoryExist, makeAbsolute)
import System.Environment.Executable (splitExecutablePath)
import System.FilePath ((</>))

getSlotsPath :: IO FilePath
getSlotsPath = do
  (execPath, _) <- splitExecutablePath
  exists <- doesDirectoryExist (execPath </> "slots")
  return $ if exists
      then execPath </> "slots"
      else "slots"
