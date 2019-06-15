module Data.SlotItem
  ( SlotItem(..)
  )
where

import Data.Text

data SlotItem = ImageItem FilePath
              | TextItem Text
  deriving Show
