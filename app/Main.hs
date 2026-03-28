{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Main entry point for the NDVI Analysis Backend.
--
-- This server demonstrates Haskell's lazy evaluation:
-- 1. Uploaded files are read lazily (Data.ByteString.Lazy)
-- 2. NDVI computation happens on-demand via lazy lists
-- 3. Results are streamed to the client row-by-row
-- 4. Statistics are computed via a single lazy fold

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.Wai (Middleware)
import Network.HTTP.Types.Status (status400, status404)

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeExtension)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

import Types (NdviStats(..))
import Ndvi (lazyProcessGrid, parseCSVLazy,
             parseJSONData, computeStats)

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import System.IO (hSetBuffering, stdout, BufferMode(..))

-- | Global state: path to the currently uploaded file.
--   Only one file at a time (simple for demonstration).

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "============================================"
  putStrLn "  NDVI Analysis Backend (Haskell + Scotty)"
  putStrLn "  Demonstrating Lazy Evaluation"
  putStrLn "============================================"
  portStr <- lookupEnv "PORT"
  let port = read (fromMaybe "3001" portStr)
  
  putStrLn $ "Starting server on port " ++ show port ++ "..."
  
  createDirectoryIfMissing True "uploads"
  
  currentFile <- newIORef (Nothing :: Maybe FilePath)
  
  scotty port $ do
    -- CORS middleware for React frontend
    middleware corsMiddleware

    -- Health check
    get "/health" $ do
      json $ object ["status" .= ("ok" :: String)]

    -- POST /upload — Accept file upload
    -- The file is saved to disk. NO processing happens here.
    -- Processing is deferred until GET /ndvi or GET /stats (lazy!).
    post "/upload" $ do
      -- Read raw body as the file content
      reqBody <- body
      -- Get format from query parameter, default to csv
      fmt <- param "format" `rescue` (\_ -> return ("csv" :: TL.Text))
      let extension = if fmt == "json" then ".json" else ".csv"
      let filePath  = "uploads" </> ("dataset" ++ TL.unpack extension)

      liftIO $ do
        BL.writeFile filePath reqBody
        writeIORef currentFile (Just filePath)
        putStrLn $ "File uploaded: " ++ filePath
        putStrLn $ "Size: " ++ show (BL.length reqBody) ++ " bytes"
        putStrLn "File saved. NDVI computation deferred (lazy evaluation)."

      json $ object
        [ "status"   .= ("uploaded" :: String)
        , "filename" .= filePath
        , "message"  .= ("File saved. NDVI computation will happen lazily on request." :: String)
        ]

    -- GET /ndvi — Lazily compute and stream NDVI values.
    --
    -- HOW LAZY EVALUATION WORKS HERE:
    -- 1. File content is read lazily (BL.readFile returns a lazy ByteString)
    -- 2. CSV lines are split lazily (each line parsed only when needed)
    -- 3. lazyProcessGrid maps computeNdvi lazily over the grid
    -- 4. The JSON encoding consumes the lazy list, triggering computation
    -- 5. Each NDVI value is computed exactly when it's serialized to JSON
    --
    -- If the client disconnects mid-stream, remaining values are NEVER computed.
    get "/ndvi" $ do
      mPath <- liftIO $ readIORef currentFile
      case mPath of
        Nothing -> do
          status status404
          json $ object
            [ "error" .= ("No dataset loaded" :: String) ]

        Just filePath -> do
          exists <- liftIO $ doesFileExist filePath
          if not exists
            then do
              status status404
              json $ object
                [ "error" .= ("Dataset file not found" :: String) ]
            else do
              liftIO $ putStrLn "GET /ndvi — Beginning lazy NDVI computation..."

              -- Step 1: Read file LAZILY
              content <- liftIO $ BL.readFile filePath
              liftIO $ putStrLn "  File handle opened (content NOT yet read — lazy!)"

              -- Step 2: Parse into pixel grid
              let ext = takeExtension filePath
              let pixelGrid = case ext of
                    ".csv" -> parseCSVLazy content  -- truly lazy line-by-line
                    ".json" -> case parseJSONData content of
                                 Right grid -> grid
                                 Left _     -> []
                    _ -> []

              -- Step 3: Lazily compute NDVI grid
              -- NO computation has happened yet! lazyProcessGrid returns
              -- a lazy list of lazy lists. Values are computed when consumed.
              let ndviGrid = lazyProcessGrid pixelGrid
              liftIO $ putStrLn "  NDVI grid structure created (values NOT yet computed — lazy!)"

              -- Step 4: Encode to JSON — THIS triggers the lazy computation.
              -- As each row is encoded, its NDVI values are finally computed.
              liftIO $ putStrLn "  Encoding response (computation happens NOW, row by row)..."

              json $ object
                [ "ndvi" .= ndviGrid
                , "rows" .= length ndviGrid
                , "cols" .= (if null ndviGrid then 0 else length (head ndviGrid))
                , "message" .= ("NDVI values computed lazily on demand" :: String)
                ]

    -- GET /stats — Compute statistics via single lazy fold.
    --
    -- The fold pulls values one at a time from the lazy NDVI grid.
    -- Each NDVI value is computed (from the pixel) only when the fold
    -- reaches it. This is a textbook example of lazy evaluation +
    -- strict accumulator pattern in Haskell.
    get "/stats" $ do
      mPath <- liftIO $ readIORef currentFile
      case mPath of
        Nothing -> do
          status status404
          json $ object
            [ "error" .= ("No dataset loaded" :: String) ]

        Just filePath -> do
          exists <- liftIO $ doesFileExist filePath
          if not exists
            then do
              status status404
              json $ object
                [ "error" .= ("Dataset file not found" :: String) ]
            else do
              liftIO $ putStrLn "GET /stats — Computing statistics via lazy fold..."

              content <- liftIO $ BL.readFile filePath

              let ext = takeExtension filePath
              let pixelGrid = case ext of
                    ".csv" -> parseCSVLazy content
                    ".json" -> case parseJSONData content of
                                 Right grid -> grid
                                 Left _     -> []
                    _ -> []

              -- Lazy NDVI grid — no values computed yet
              let ndviGrid = lazyProcessGrid pixelGrid

              -- computeStats uses foldl' with a strict accumulator
              -- over the lazy NDVI grid. Each value is computed on demand.
              let !stats = computeStats ndviGrid
              liftIO $ putStrLn $ "  Stats computed: " ++ show stats

              json stats

-- | CORS middleware configuration.
--   Allows the React frontend (localhost:5173) to call our API.
corsMiddleware :: Middleware
corsMiddleware = cors $ const $ Just CorsResourcePolicy
  { corsOrigins        = Nothing  -- Allow all origins (dev mode)
  , corsMethods        = ["GET", "POST", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Just 3600
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }
