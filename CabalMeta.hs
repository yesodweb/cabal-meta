{-# LANGUAGE CPP, OverloadedStrings #-}
module CabalMeta where

-- TODO:
-- * use --only-dependencies. otherwise a simple build failure will prevent a cabal-src-install
-- support git & tar.gz urls

import Shelly
import Prelude hiding (FilePath)
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text.Lazy as LT
import Control.Monad (forM)
import Data.Monoid (Monoid(..))
import Filesystem.Path.CurrentOS (FilePath, hasExtension)
{--
import FileLocation (debug)
--}
#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
import Control.Monad (when)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#else
import Data.Monoid ((<>))
#endif

source_file :: FilePath
source_file = "sources.txt"

cabal_install :: [Text] -> ShIO Text
cabal_install = command "cabal" ["install"]

cabal_install_ :: [Text] -> ShIO ()
cabal_install_ = command_ "cabal" ["install"]

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x

data Package = Directory {
    dLocation :: FilePath
  , pFlags :: [Text]
} | Package {
    pLocation :: Text
  , pFlags :: [Text]
} deriving Show

asList :: Package -> [Text]
asList (Package l flags) = l:flags
asList (Directory d flags) = toTextUnsafe d : flags

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

readPackages :: Bool -> FilePath -> ShIO PackageSources
readPackages allowCabals startDir = do
  fullDir <- path startDir
  chdir fullDir $ do
    cabalPresent <- if allowCabals then return False else isCabalPresent
    if cabalPresent then return mempty else do
        psources <- getSources
        when (null (dirs psources) && null (hackages psources)) $ terror $ "empty " <>| source_file

        child_pkgs <- forM (dirs psources) $ \dir -> do
          b <- fmap (== fullDir) (path $ dLocation dir)
          if b  then return mempty else readPackages False (dLocation dir)
        return $ mempty {
            hackages = hackages psources ++ concatMap hackages child_pkgs
          , dirs =
              concatMap (\(p, ps) -> if null ps then [p] else ps) $
                zip (dirs psources) (map dirs child_pkgs)
          }
  where
    isCabalFile = flip hasExtension "cabal"
    isCabalPresent = fmap (any isCabalFile) (ls ".")

    terror = fail . unpack

    getSources :: ShIO PackageSources
    getSources = do
        sourceLines <- fmap LT.lines $ readfile source_file
        let sources = paritionSources [ source | 
              source <- map LT.words sourceLines,
              not . null $ source
              ]
        ds <- mapM fullPath (dirs sources)
        return $ sources { dirs = ds }
      where
        fullPath package = do
          fp <- path $ dLocation package
          return package { dLocation = fp }

        paritionSources :: [[Text]] -> PackageSources
        paritionSources = go mempty
          where
          go sources [] = sources
          go _ ([]:_) = error "impossible"
          go sources ((name:flags):more) = let c = LT.head name in
            if c == '.' || c == '/'               then go sources { dirs = mkDir: dirs sources } more
              else if "http" `LT.isPrefixOf` name then go sources { https = mkPkg: https sources } more
              else if "git:" `LT.isPrefixOf` name then go sources { gits = mkPkg: gits sources } more
              else                                     go sources { hackages = mkPkg: hackages sources } more
            where
              mkDir = Directory (fromText name) flags
              mkPkg = Package name flags
