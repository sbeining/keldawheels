{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.SlotMachine as SlotMachine
import qualified Data.SlotItem as SlotItem
import qualified Data.Text as T
import System.Directory
import System.Environment.Executable
import System.FilePath
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
  -- search in directory of the executable first and then in the current directory
  (execPath, _) <- splitExecutablePath
  exists <- doesDirectoryExist (execPath </> "slots")
  let path = if exists
      then execPath </> "slots"
      else "slots"

  machine <- SlotMachine.fromFilePath path

  startGUI defaultConfig (setup machine)

setup :: SlotMachine.SlotMachine -> Window -> UI ()
setup machine window = void $ do
  _ <- return window # set UI.title "Lucky Wheels"

  -- GUI Elements
  slots <- buildSlotMachineWidget machine
  button <- UI.button #+ [string "Roll All!"]
  result <- UI.div

  -- Events
  on UI.click button $ const $ do
    slotItems <- liftIO $ SlotMachine.spin machine
    let winners = buildSlotItemWidget <$> slotItems
    _ <- element result # set UI.children []
    element result #+ winners

  -- DOM
  getBody window #+
    [ element slots
    , element result
    , element button
    ]

  where
    buildSlotMachineWidget :: SlotMachine.SlotMachine -> UI Element
    buildSlotMachineWidget (SlotMachine.SlotMachine slots) = UI.div #+ (buildSlotWidget <$> slots)

    buildSlotWidget :: SlotMachine.Slot -> UI Element
    buildSlotWidget (SlotMachine.Slot items) = do
      UI.ul #+ (buildSlotItemWidget <$> items)

    buildSlotItemWidget :: SlotMachine.SlotItem -> UI Element
    buildSlotItemWidget (SlotMachine.TextItem content) = UI.li #+ [string $ T.unpack content]
    buildSlotItemWidget item = do
      src <- liftIO $ SlotItem.toHtmlSrc item
      UI.li #+
        [ UI.img # set UI.src src ]
