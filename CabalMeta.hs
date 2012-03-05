{-# LANGUAGE OverloadedStrings #-}
module CabalMeta where

-- TODO:
-- * use --only-dependencies. otherwise a simple build failure will prevent a cabal-src-install
-- support git & tar.gz urls

import Shelly
import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text.Lazy as LT
import Control.Monad (forM, when)

{-
import FileLocation (debug)
-}

source_file :: FilePath
source_file = "sources.txt"

cabal_install :: [Text] -> ShIO Text
cabal_install = command "cabal" ["install"]

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x

data Package = Package {
    pLocation :: Text
  , pFlags :: [Text]
} deriving Show

asList :: Package -> [Text]
asList (Package l flags) = l:flags

data PackageSources = PackageSources {
    dirs     :: [Package]
  , hackages :: [Package]
  , https    :: [Package]
  , gits     :: [Package]
} deriving Show

readPackages :: Bool -> Text -> ShIO [Package]
readPackages allowCabals dir = do
  fullDir <- path dir
  chdir fullDir $ do
    cabalPresent <- if allowCabals then return False else isCabalPresent
    if cabalPresent then return [] else do
        packages <- getSources
        let allPkgs = dirs packages ++ hackages packages
        let ds = dirs packages
        when (null allPkgs) $ terror $ "empty " <> source_file

        child_pkgs <- forM ds $ \pkg -> do
          b <- fmap (== fullDir) (path $ pLocation pkg)
          if b  then return [] else readPackages False (pLocation pkg)
        return $ hackages packages ++
          (concatMap (\(p, ps) -> if null ps then [p] else ps) $ zip ds child_pkgs)
  where
    isCabalFile = LT.isSuffixOf ".cabal" 
    isCabalPresent = do
        cabals <- fmap (filter isCabalFile) $ ls "."
        return $ not $ null cabals 

    terror = fail . unpack

    getSources :: ShIO PackageSources
    getSources = do
        sourceLines <- fmap LT.lines $ readfile source_file
        let sources = paritionSources $ [ source | 
              source <- map LT.words sourceLines,
              not . null $ source
              ]
        ds <- mapM fullPath (dirs sources)
        return $ sources { dirs = ds }
      where
        fullPath package = do
          fp <- path $ pLocation package
          return package { pLocation = fp }

        paritionSources :: [[Text]] -> PackageSources
        paritionSources = go $ PackageSources [] [] [] []
          where
          go sources [] = sources
          go _ ([]:_) = error "impossible"
          go sources ((name:flags):more) = let c = LT.head name in
            if c == '.' || c == '/'              then go sources { dirs = package: dirs sources } more
              else if LT.isPrefixOf "http:" name then go sources { https = package: https sources } more
              else if LT.isPrefixOf "git:"  name then go sources { gits = package: gits sources } more
              else                                    go sources { hackages = package: hackages sources } more
            where
              package = Package name flags
