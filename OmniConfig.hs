{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}
module OmniConfig (
 OptionsSource(..), allProgramOpts, commandLine, environment, homeOptFile
 , checkNegatedOpt
) where

import Prelude hiding (FilePath)
import System.Environment
import Control.Exception (SomeException, handle)
import qualified Data.Text as T
import Data.Text (Text)

import Filesystem.Path.CurrentOS
import Filesystem (readTextFile, getHomeDirectory)

allProgramOpts :: [OptionsSource] -> IO [Text]
allProgramOpts confs = do
  fmap Prelude.concat $ mapM loadProgramOptions confs

checkNegatedOpt :: Text -> [Text] -> (Maybe Bool, [Text])
checkNegatedOpt optText args =
    if elem negatedOpt args
      then (Just False, filteredArgs)
      else if elem opt args
             then (Just True, filteredArgs)
             else (Nothing, args)
  where
    filteredArgs = filter (\arg -> arg /= negatedOpt && arg /= opt) args
    opt = "--" `T.append` optText
    negatedOpt = "--no-" `T.append` optText

data OptionsSource = OptionsSource {
    loadProgramOptions :: IO [Text]
  }

commandLine :: OptionsSource
commandLine = OptionsSource {
    loadProgramOptions = fmap (map T.pack) getArgs
  }

environment :: Text -> OptionsSource
environment name = OptionsSource {
    loadProgramOptions = do
      env <- getEnvironment
      return $ case lookup (T.unpack $ T.replace "-" "_" $ T.toUpper name `T.append` "_OPTS") env of
                Nothing -> []
                Just s -> T.splitOn " " $ T.pack s
  }

homeOptFile :: Text -> OptionsSource
homeOptFile name = OptionsSource {
    loadProgramOptions  = do
      hd <- getHomeDirectory
      contents <- handle (\(_::SomeException) -> return "") $
        readTextFile $ hd </> fromText ("." `T.append` name) </> "opts"
      return $ T.splitOn " " contents
  }
