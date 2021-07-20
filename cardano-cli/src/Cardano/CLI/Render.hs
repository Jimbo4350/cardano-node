{-# LANGUAGE OverloadedStrings #-}

module Cardano.CLI.Render
  ( renderHtmlShowS
  ) where

import Data.Function
import Options.Applicative.Help.Ann
import Prettyprinter
import Prettyprinter.Render.Util.Panic
import Text.Show

import qualified Data.List as L
import qualified Data.Text as T

renderOpenAnn :: Ann -> ShowS
renderOpenAnn ann = case ann of
  AnnTrace s -> id
    . ("<span trace=" <>)
    . (show s <>)
    . (">" <>)

renderCloseAnn :: ShowS
renderCloseAnn = (<> "</span>")

-- | Render a 'SimpleDocStream' to a 'ShowS', useful to write 'Show' instances
-- based on the prettyprinter.
--
-- @
-- instance 'Show' MyType where
--     'showsPrec' _ = 'renderHtmlShowS' . 'layoutPretty' 'defaultLayoutOptions' . 'pretty'
-- @
renderHtmlShowS :: SimpleDocStream Ann -> ShowS
renderHtmlShowS ds = id
  . ("<html>\n" <>)
  . ("<body>\n" <>)
  . ("<pre>\n" <>)
  . (<> "\n</pre>")
  . (<> "\n</body>")
  . (<> "\n</html>")
  . go ds
  where
    go :: SimpleDocStream Ann -> ShowS
    go sds = case sds of
      SFail           -> panicUncaughtFail
      SEmpty          -> id
      SChar c x       -> showChar c . go x
      SText _l t x    -> showString (T.unpack t) . go x
      SLine i x       -> showString ('\n' : L.replicate i ' ') . go x
      SAnnPush ann x  -> renderOpenAnn ann . go x
      SAnnPop x       -> go x . renderCloseAnn
