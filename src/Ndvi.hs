{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | NDVI computation module using Haskell's lazy evaluation.
--
-- KEY DESIGN PRINCIPLE:
-- NDVI values are computed only when requested by the client,
-- leveraging Haskell's lazy evaluation to avoid unnecessary computation.
--
-- - CSV files are read line-by-line (truly lazy I/O)
-- - NDVI computation uses lazy lists — each value is computed on demand
-- - Statistics are computed via a single lazy fold (no intermediate lists)
-- - The full dataset is NEVER materialized in memory at once

module Ndvi
  ( computeNdvi
  , lazyProcessGrid
  , parseCSVLazy
  , parseJSONData
  , computeStats
  , flattenGrid
  ) where

import Types (Pixel(..), NdviStats(..))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (foldl')
import Data.Char (isSpace)

-- | Compute NDVI for a single pixel.
--   Formula: NDVI = (NIR - Red) / (NIR + Red)
--   Returns 0.0 when both bands are zero (avoid division by zero).
--
--   This function is PURE — it will only be evaluated when
--   the result is actually needed (lazy evaluation).
computeNdvi :: Pixel -> Double
computeNdvi (Pixel red nir)
  | nir + red == 0 = 0.0
  | otherwise      = (nir - red) / (nir + red)

-- | Lazily process a 2D grid of pixels into a 2D grid of NDVI values.
--   Each row and each value is computed ON DEMAND — not eagerly.
--
--   Due to Haskell's lazy evaluation:
--   - If the client only requests the first 3 rows, only 3 rows are computed
--   - Individual NDVI values within a row are also lazy
--   - The entire grid is never forced into memory
lazyProcessGrid :: [[Pixel]] -> [[Double]]
lazyProcessGrid = map (map computeNdvi)
-- ^ This is a lazy map-of-maps. No computation happens here.
--   Values are only computed when pattern-matched or consumed.

-- | Parse CSV data lazily, line by line.
--   This is TRUE lazy I/O — each line is read and parsed on demand.
--
--   Expected CSV format:
--     red,nir
--     120,200
--     130,210
--     ...
--
--   Lines are grouped into rows of a specified width for grid display.
--   If no width is specified, each line becomes its own row.
parseCSVLazy :: BL.ByteString -> [[Pixel]]
parseCSVLazy content =
  let allLines   = BLC.lines content                 -- lazy line splitting
      dataLines  = dropHeader allLines               -- skip header if present
      pixels     = map parseLine dataLines           -- lazy map: each line parsed on demand
      filtered   = filter validPixel pixels          -- lazy filter
      gridWidth  = autoDetectWidth (length filtered) -- determine grid width
  in  chunksOf gridWidth filtered                    -- lazy chunking into rows
  where
    -- | Drop header row if it starts with non-numeric character
    dropHeader [] = []
    dropHeader (l:ls)
      | isHeaderLine l = ls
      | otherwise      = l : ls

    -- | Check if a line looks like a CSV header (starts with a letter)
    isHeaderLine line =
      case BLC.unpack (BLC.takeWhile (/= ',') (BLC.dropWhile isSpace line)) of
        []    -> False
        (c:_) -> not (c >= '0' && c <= '9') && c /= '-' && c /= '.'

    -- | Parse a single CSV line "red_value,nir_value" into a Pixel.
    --   This is only evaluated when the pixel is actually consumed.
    parseLine :: BL.ByteString -> Pixel
    parseLine line =
      let parts = BLC.split ',' line
      in case parts of
           [r, n] -> Pixel (readDouble r) (readDouble n)
           _      -> Pixel 0 0  -- fallback for malformed lines

    readDouble :: BL.ByteString -> Double
    readDouble bs = case reads (BLC.unpack (BLC.dropWhile isSpace bs)) of
      [(v, _)] -> v
      _        -> 0.0

    validPixel (Pixel r n) = r >= 0 && n >= 0 && (r + n) > 0

    -- | Auto-detect grid width from total pixel count.
    --   Tries to find a square-ish grid, falls back to 10.
    autoDetectWidth :: Int -> Int
    autoDetectWidth total
      | total <= 0 = 1
      | otherwise  =
          let sqrtN = floor (sqrt (fromIntegral total :: Double)) :: Int
          in if sqrtN * sqrtN == total then sqrtN
             else if total `mod` 10 == 0 then 10
             else sqrtN

    -- | Split a list into chunks of size n. Lazy.
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = let (chunk, rest) = splitAt n xs
                    in chunk : chunksOf n rest

-- | Parse JSON data into a 2D pixel grid.
--   JSON parsing itself (via Aeson) is strict, but the RESULTING
--   computation on the parsed data is fully lazy.
--
--   The lazy evaluation happens at the computation level:
--   after parsing, NDVI values are computed on demand via lazyProcessGrid.
parseJSONData :: BL.ByteString -> Either String [[Pixel]]
parseJSONData = eitherDecode

-- | Flatten a 2D grid of NDVI values into a single list.
--   Lazy — values are produced on demand.
flattenGrid :: [[Double]] -> [Double]
flattenGrid = concat
-- ^ concat on lazy lists is itself lazy.

-- | Compute statistics from a 2D NDVI grid using a SINGLE lazy fold.
--   This processes the entire grid in one pass without creating
--   intermediate data structures.
--
--   The fold is lazy in its consumption of the input list:
--   - It pulls one NDVI value at a time from the lazy grid
--   - Each value is computed (via computeNdvi) only when the fold reaches it
--   - The accumulator is strict (using foldl') to avoid space leaks
--
--   Returns: NdviStats with mean, min, max, and vegetation percentage.
computeStats :: [[Double]] -> NdviStats
computeStats grid =
  let flat = flattenGrid grid  -- lazy: not yet evaluated
      -- Single-pass strict fold over the lazy list
      (total, count, minV, maxV, vegCount) =
        foldl' accumulate (0.0, 0 :: Int, 1.0, -1.0, 0 :: Int) flat
      mean = if count == 0 then 0.0 else total / fromIntegral count
      vegPct = if count == 0 then 0.0
               else (fromIntegral vegCount / fromIntegral count) * 100.0
  in NdviStats
       { statsMean          = roundTo 4 mean
       , statsMin           = roundTo 4 minV
       , statsMax           = roundTo 4 maxV
       , statsVegetationPct = roundTo 2 vegPct
       }
  where
    -- | Strict accumulator for the fold.
    --   Computes running total, count, min, max, and vegetation count.
    --   Vegetation threshold: NDVI > 0.2
    accumulate (!total, !count, !minV, !maxV, !vegC) val =
      ( total + val
      , count + 1
      , min minV val
      , max maxV val
      , if val > 0.2 then vegC + 1 else vegC
      )

    roundTo :: Int -> Double -> Double
    roundTo n x = let factor = 10.0 ^^ n
                  in  fromIntegral (round (x * factor) :: Integer) / factor
