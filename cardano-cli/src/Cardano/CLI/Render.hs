module Cardano.CLI.Render
  ( renderHtmlShowS
  ) where

import Data.Function
import Prettyprinter
import Prettyprinter.Render.Util.Panic
import Text.Show

import qualified Data.List as L
import qualified Data.Text as T

-- | Render a 'SimpleDocStream' to a 'ShowS', useful to write 'Show' instances
-- based on the prettyprinter.
--
-- @
-- instance 'Show' MyType where
--     'showsPrec' _ = 'renderHtmlShowS' . 'layoutPretty' 'defaultLayoutOptions' . 'pretty'
-- @
renderHtmlShowS :: SimpleDocStream ann -> ShowS
renderHtmlShowS = go
  where
    go :: SimpleDocStream ann -> ShowS
    go sds = case sds of
      SFail        -> panicUncaughtFail
      SEmpty       -> id
      SChar c x    -> showChar c . go x
      SText _l t x -> showString (T.unpack t) . go x
      SLine i x    -> showString ('\n' : L.replicate i ' ') . go x
      SAnnPush _ x -> go x
      SAnnPop x    -> go x
