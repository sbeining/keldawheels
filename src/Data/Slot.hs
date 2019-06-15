module Data.Slot
  ( Slot(..)
  )
where

import Data.SlotItem

data Slot = Slot [SlotItem]
  deriving Show
