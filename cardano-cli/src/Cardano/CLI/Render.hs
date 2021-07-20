{-# LANGUAGE OverloadedStrings #-}

module Cardano.CLI.Render
  ( renderHtmlShowS
  ) where

import Data.Function
import Data.Ord
import Options.Applicative.Help.Ann
import Prettyprinter
import Prettyprinter.Render.Util.Panic
import Text.Show

import qualified Data.List as L
import qualified Data.Text as T

renderOpenAnn :: Ann -> ShowS
renderOpenAnn ann = case ann of
  AnnTrace _ s -> id
    . ("<span trace=" <>)
    . (show s <>)
    . (">" <>)

renderCloseAnn :: Ann -> ShowS
renderCloseAnn ann = case ann of
  AnnTrace _ _ -> id
    . (<> "</span>")

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
  . go [] ds
  where
    go :: [Ann] -> SimpleDocStream Ann -> ShowS
    go anns sds = case sds of
      SFail           -> panicUncaughtFail
      SEmpty          -> id
      SChar c x       -> showChar c . go anns x
      SText _l t x    -> showString (T.unpack t) . go anns x
      SLine i x       -> showString ('\n' : L.replicate i ' ') . go anns x
      SAnnPush ann x  -> onLevel ann (renderOpenAnn ann) . go (ann:anns) x
      SAnnPop x       -> case anns of
        (a:as) -> go as x . onLevel a (renderCloseAnn a)
        [] -> go [] x
    onLevel :: Ann -> ShowS -> ShowS
    onLevel (AnnTrace n _) f = if n >= 2 then f else id
