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
  let extension = takeExtension path
  return $ case extension of
    ".png" -> "image/png"
    ".jpg" -> "image/jpeg"
    ".jpe" -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".gif" -> "image/gif"
    ".svg" -> "image/svg+xml"
    _ -> error "FileType is not supported"
toMimeType (TextItem _) = error "This should not be called on a TextItem"

toBase64 :: SlotItem -> IO BS.ByteString
toBase64 (ImageItem path) = do
  content <- BS.readFile path

  return $ encode content
toBase64 (TextItem _) = error "This should not be called on a TextItem"
