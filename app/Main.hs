{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.SlotMachine as SlotMachine
import qualified Data.SlotItem as SlotItem
import qualified Data.Text as T
import Paths
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
  static <- getStaticPath
  path <- getSlotsPath
  machine <- SlotMachine.fromFilePath path

  startGUI defaultConfig
    { jsStatic = Just static
    } $ setup machine

setup :: SlotMachine.SlotMachine -> Window -> UI ()
setup machine window = void $ do
  _ <- return window # set UI.title "Lucky Wheels"
  UI.addStyleSheet window "styles.css"

  -- GUI Elements
  slots <- buildSlotMachineWidget machine
  button <- UI.button #+ [string "Roll All!"]
  result <- UI.ul #. "result"

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
    buildSlotMachineWidget (SlotMachine.SlotMachine slots) =
      UI.div #. "slot-machine" #+ (buildSlotWidget <$> slots)

    buildSlotWidget :: SlotMachine.Slot -> UI Element
    buildSlotWidget (SlotMachine.Slot items) = do
      UI.ul #. "slot" #+ (buildSlotItemWidget <$> items)

    buildSlotItemWidget :: SlotMachine.SlotItem -> UI Element
    buildSlotItemWidget (SlotMachine.TextItem content) =
      UI.li #. "text" #+ [string $ T.unpack content]

    buildSlotItemWidget item = do
      src <- liftIO $ SlotItem.toHtmlSrc item
      UI.li #+
        [ UI.img # set UI.src src ]
