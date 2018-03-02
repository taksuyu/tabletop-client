{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens
import Data.Binary.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)

import Development.Shake
import Development.Shake.FilePath

import Heist as H
import Heist.Compiled as HC

dist :: FilePath
dist = "dist"

distFiles :: [FilePath]
distFiles = fmap (dist </>) $ [ "index.html" ]
  `mappend` fmap ("games" </>) [ "ur.html" ]
  `mappend` fmap ("css" </>) [ "main.css", "ur.css", "index.css" ]
  -- `mappend` fmap ("svg" </>) []
  `mappend` fmap ("js" </>) [ "ur.bundle.js" ]

templates :: H.HeistConfig m
templates = H.emptyHeistConfig
    & H.hcNamespace .~ mempty
    & H.hcLoadTimeSplices .~ H.defaultLoadTimeSplices
    & H.hcTemplateLocations .~ [H.loadTemplates "templates"]

main :: IO ()
main = do
  heist <- H.initHeist templates

  shakeArgs shakeOptions $ do
    want distFiles

    phony "clean" $ do
      putNormal "Cleaning files in dist/"
      removeFilesAfter "dist" ["//*"]

    dist <//> "*.html" %> \out -> do
      let templateName = dropDirectory1 $ out -<.> ""

      -- NOTE: We want to track the templates so that they get rebuilt on
      -- changes
      files <- getDirectoryFiles "" ["templates//*.tpl"]
      need files

      result <- case heist of
        Right hs ->
          case HC.renderTemplate hs $ BS8.pack templateName of
            Just (a, _) -> do
              liftIO . LBS.writeFile out . BB.toLazyByteString =<< a
              pure True
            _ -> pure False
        _ -> pure False

      if result
        then putNormal ("Built template: " `mappend` out)
        else putLoud ("Could not build: " `mappend` out)

    dist </> "css" </> "*.css" %> \out -> do
      let name = dropDirectory1 out
      need [ name ]
      putNormal $ "# copy (for " `mappend` out `mappend` " )"
      copyFileChanged name out

    dist </> "svg" </> "*.svg" %> \out -> do
      let name = dropDirectory1 out
      need [ name ]
      copyFileChanged name out

    "output" <//> "*.js" %> \_ -> do
      pursFiles <- getDirectoryFiles "" ["src//*.purs"]
      need pursFiles
      unit $ cmd "stack exec psc-package build"

    dist </> "js" </> "*.bundle.js" %> \out -> do
      let moduleName = toUpperCase $ takeBaseName $ out -<.> ""

      need [ "output" </> moduleName </> "index.js" ]

      unit $ cmd "stack exec -- purs bundle output/**/*.js --main" moduleName "-m" moduleName "-o" out

-- NOTE: Doesn't properly minify html
-- stripWhiteSpace :: LBS.ByteString -> LBS.ByteString
-- stripWhiteSpace = LBS.filter (\c -> c /= fromIntegral (fromEnum ' ') && c /= fromIntegral (fromEnum '\n'))

-- Applies `toUpper` from "Data.Char" to the first character of a String.
toUpperCase :: String -> String
toUpperCase = \case
  [] -> []
  (x:xs) -> toUpper x : xs
