module Data.SlotItem
  ( SlotItem(..)
  , fromFilePath
  , toHtmlSrc
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base64
import qualified Data.Text as T
import System.FilePath
import Magic

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

toHtmlSrc :: SlotItem -> IO String
toHtmlSrc imageItem = do
  mime <- toMimeType imageItem
  content <- toBase64 imageItem

  return $ "data:" <> mime <> ";base64," <> C.unpack content

toMimeType :: SlotItem -> IO String
toMimeType (ImageItem path) = do
  magic <- magicOpen [MagicMime]
  magicLoadDefault magic
  magicFile magic path
toMimeType (TextItem _) = error "This should not be called on a TextItem"

toBase64 :: SlotItem -> IO BS.ByteString
toBase64 (ImageItem path) = do
  content <- BS.readFile path

  return $ encode content
toBase64 (TextItem _) = error "This should not be called on a TextItem"
