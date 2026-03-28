{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Pixel(..)
  , NdviStats(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), withObject)

-- | A single pixel with Red and NIR band values.
--   Represents one data point from the uploaded dataset.
data Pixel = Pixel
  { pixelRed :: !Double
  , pixelNir :: !Double
  } deriving (Show, Generic)

instance FromJSON Pixel where
  parseJSON = withObject "Pixel" $ \v -> Pixel
    <$> v .: "red"
    <*> v .: "nir"

instance ToJSON Pixel where
  toJSON p = object
    [ "red" .= pixelRed p
    , "nir" .= pixelNir p
    ]

-- | Aggregated NDVI statistics computed lazily via single-pass fold.
data NdviStats = NdviStats
  { statsMean           :: !Double
  , statsMin            :: !Double
  , statsMax            :: !Double
  , statsVegetationPct  :: !Double  -- ^ Percentage of pixels with NDVI > 0.2
  } deriving (Show, Generic)

instance ToJSON NdviStats where
  toJSON s = object
    [ "mean"             .= statsMean s
    , "min"              .= statsMin s
    , "max"              .= statsMax s
    , "vegetationPercent" .= statsVegetationPct s
    ]
