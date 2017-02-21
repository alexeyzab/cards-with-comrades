module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM.Event.Types (Event ())

foreign import allowDrop :: Event -> Event
foreign import drag :: Event -> Event
foreign import drop :: Event -> Event

main = do
  log "Hello sailor!"
