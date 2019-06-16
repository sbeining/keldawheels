module UI
  ( addScriptFile
  )
where

import Control.Monad (void)
import Graphics.UI.Threepenny.Core

addScriptFile :: Window -> FilePath -> UI ()
addScriptFile w filename = void $ do
    el <- mkElement "script"
            # set (attr "src") ("/static/js/" ++ filename)
    getHead w #+ [element el]
