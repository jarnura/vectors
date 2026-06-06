-- HTML *overlay* text animation via anime.js (DOM only — never WebGL).
-- Wraps anime.js scrambleText to animate an overlay element's text content.
module Text
  ( scrambleInto
  , setVisible
  ) where

import Prelude (Unit)
import Effect (Effect)

-- Scramble the text of the overlay element with the given id into `text`.
-- No-op if the element is absent. DOM only.
foreign import scrambleInto :: String -> String -> Effect Unit

-- Show or hide the overlay element with the given id.
foreign import setVisible :: String -> Boolean -> Effect Unit
