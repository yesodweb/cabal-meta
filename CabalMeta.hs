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
import Data.Monoid (Monoid(..))

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

instance Monoid PackageSources where
  mempty = PackageSources [] [] [] []
  mappend (PackageSources d1 ha1 ht1 g1) (PackageSources d2 ha2 ht2 g2) =
    PackageSources (mappend d1 d2) (mappend ha1 ha2)
      (mappend ht1 ht2) (mappend g1 g2)

readPackages :: Bool -> Text -> ShIO PackageSources
readPackages allowCabals dir = do
  fullDir <- path dir
  chdir fullDir $ do
    cabalPresent <- if allowCabals then return False else isCabalPresent
    if cabalPresent then return mempty else do
        psources <- getSources
        let allPkgs = dirs psources ++ hackages psources
        when (null allPkgs) $ terror $ "empty " <> source_file

        child_pkgs <- forM (dirs psources) $ \pkg -> do
          b <- fmap (== fullDir) (path $ pLocation pkg)
          if b  then return mempty else readPackages False (pLocation pkg)
        return $ mempty {
            hackages = hackages psources ++ concatMap hackages child_pkgs
          , dirs =
              concatMap (\(p, ps) -> if null ps then [p] else ps) $
                zip (dirs psources) (map dirs child_pkgs)
          }
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
        paritionSources = go mempty
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
