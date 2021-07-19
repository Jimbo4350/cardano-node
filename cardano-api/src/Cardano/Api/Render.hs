-- | Text rendering of various API types
--
module Cardano.Api.Render
  ( renderPrettyValue
  , renderValue
  ) where

import           Prelude

import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api.SerialiseRaw
import           Cardano.Api.Value

-- | Render a textual representation of a 'Value'.
--
-- Note that this textual representation can be parsed by 'parseValue'.
renderValue :: Value -> Text
renderValue  v =
    Text.intercalate
      " + "
      (map renderAssetIdQuantityPair vals)
  where
    vals :: [(AssetId, Quantity)]
    vals = valueToList v

-- | Render a \"prettified\" textual representation of a 'Value'.
renderPrettyValue :: Value -> Text
renderPrettyValue v =
    Text.intercalate
      ("\n" <> Text.replicate 4 " " <> "+ ")
      (map renderAssetIdQuantityPair vals)
  where
    vals :: [(AssetId, Quantity)]
    vals = valueToList v

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------


renderPolicyId :: PolicyId -> Text
renderPolicyId (PolicyId scriptHash) = serialiseToRawBytesHexText scriptHash

renderAssetId :: AssetId -> Text
renderAssetId AdaAssetId = "lovelace"
renderAssetId (AssetId polId (AssetName assetName))
  | BS.null assetName = renderPolicyId polId
  | otherwise = renderPolicyId polId <> "." <> Text.decodeUtf8 assetName

renderAssetIdQuantityPair :: (AssetId, Quantity) -> Text
renderAssetIdQuantityPair (aId, quant) =
  Text.pack (show quant) <> " " <> renderAssetId aId

