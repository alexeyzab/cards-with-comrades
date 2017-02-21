module Main where

import Import
import DOMHelper
import Sortable (installSortable)

type App = forall eff. Eff (dom :: DOM, console :: CONSOLE) Unit

-- | Unsafe version of `fromJust`
unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = unsafePartial fromJust

main :: App
main = do
    doc <- getDocument
    -- install sortable
    ulAvailable <- unsafeFromJust <$> getElementById' "player-hand" doc
    installSortable ulAvailable (pure unit)
