{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}
module OmniConfig (
 ProgramConfig(..), checkProgramConfigs, commandLine, environment, homeOptFile
) where

import Prelude hiding (FilePath)
import System.Environment
import Control.Exception (SomeException, handle)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid (mappend)
import Data.Maybe (catMaybes)

import Filesystem.Path.CurrentOS
import Filesystem (readTextFile, getHomeDirectory)

checkProgramConfigs :: Text -> Text -> [ProgramConfig] -> IO (Maybe Bool)
checkProgramConfigs name key confs = do
  res <- mapM (checkProgramConfig name key) confs
  return $ case catMaybes res of
    [] -> Nothing
    (x:_) -> Just x

checkProgramConfig :: Text -> Text -> ProgramConfig -> IO (Maybe Bool)
checkProgramConfig name key conf =
    case conf of
      (ProgramConfig lpc pcf pct) -> do
        loaded <- lpc name
        return $ if pcf loaded key then Just False else
            if pct loaded key then Just True else Nothing


data ProgramConfig = forall a. ProgramConfig {
    loadProgramConfig  :: Text -> IO a
  , programConfigTrue  :: a -> Text -> Bool
  , programConfigFalse :: a -> Text -> Bool
  }

commandLine :: ProgramConfig
commandLine = ProgramConfig {
    loadProgramConfig = const $ fmap (map T.pack) getArgs
  , programConfigTrue  = \conf -> flip elem conf . keyArg
  , programConfigFalse = \conf -> flip elem conf . noKeyArg
  }

environment :: ProgramConfig
environment = ProgramConfig {
    programConfigTrue  = checkTextOpt keyArg
  , programConfigFalse = checkTextOpt noKeyArg
  , loadProgramConfig  = \name -> do
      env <- getEnvironment
      return $ case lookup (T.unpack $ T.replace "-" "_" $ T.toUpper name `mappend` "_OPTS") env of
        Nothing -> ""
        Just s -> T.pack s
  }

homeOptFile :: ProgramConfig
homeOptFile = ProgramConfig {
    programConfigTrue  = checkTextOpt keyArg
  , programConfigFalse = checkTextOpt noKeyArg
  , loadProgramConfig  = \name -> do
      hd <- getHomeDirectory
      handle (\(_::SomeException) -> return "") $
        readTextFile $ hd </> fromText ("." `mappend` name) </> "opts"
  }

keyArg, noKeyArg :: Text -> Text
keyArg   = ("--"    `mappend`)
noKeyArg = ("--no-" `mappend`)

checkTextOpt :: (Text -> Text) -> Text -> Text -> Bool
checkTextOpt optMaker = T.isInfixOf . optMaker
