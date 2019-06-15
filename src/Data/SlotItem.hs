module Data.SlotItem
  ( SlotItem(..)
  , fromFilePath
  )
where

import qualified Data.Text as T
import System.FilePath

data SlotItem = ImageItem FilePath
              | TextItem T.Text
  deriving Show

fromFilePath :: FilePath -> IO SlotItem
fromFilePath path =
  case takeExtension path of
    ".txt" -> do
      content <- readFile path
      return $ TextItem <$> T.strip $ T.pack $ content
    _ -> return $ ImageItem path
