{-# LANGUAGE CPP, OverloadedStrings #-}
module CabalMeta where

-- TODO:
-- * use --only-dependencies. otherwise a simple build failure will prevent a cabal-src-install
-- support git & tar.gz urls

import Shelly
import Prelude hiding (FilePath)
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text.Lazy as T
import Filesystem.Path.CurrentOS (FilePath, hasExtension, filename)
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (partition)

#if __GLASGOW_HASKELL__ < 704
import Data.Monoid (Monoid(..))
import Control.Monad (when, forM)
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#else
import Control.Monad (forM)
import Data.Monoid ((<>),Monoid(..))
#endif

{--
import FileLocation (debug)
--}

source_file :: FilePath
source_file = "sources.txt"

data Package = Directory {
    dLocation :: FilePath
  , pFlags :: [Text]
} | Package {
    pLocation :: Text
  , pFlags :: [Text]
} | GitPackage {
    gLocation :: Text
  , pFlags :: [Text]
  , gTag :: Maybe Text
} deriving (Show, Eq)

asList :: Package -> [Text]
asList (Package l flags) = l:flags
asList (GitPackage l flags tag) = l : flags ++ maybeToList tag
asList (Directory d flags) = toTextUnsafe d : flags

data PackageSources = PackageSources {
    dirs     :: [Package]
  , hackages :: [Package]
  , https    :: [Package] -- also git for now 
  , gits     :: [Package]
} deriving (Show, Eq)

packageList :: PackageSources -> [[Text]]
packageList = map asList . packages

packages :: PackageSources -> [Package]
packages psources =
  dirs psources ++
  hackages psources ++
  gitPackages psources

gitPackages :: PackageSources -> [Package]
gitPackages psources =
  gits psources ++ https psources

instance Monoid PackageSources where
  mempty = PackageSources [] [] [] []
  mappend (PackageSources d1 ha1 ht1 g1) (PackageSources d2 ha2 ht2 g2) =
    PackageSources (mappend d1 d2) (mappend ha1 ha2)
      (mappend ht1 ht2) (mappend g1 g2)

vendor_dir :: FilePath
vendor_dir = "vendor"

git_ :: Text -> [Text] -> ShIO ()
git_ = command1_ "git" []

readPackages :: Bool ->  FilePath -> ShIO PackageSources
readPackages allowCabals startDir = do
  fullDir <- path startDir
  chdir fullDir $ do
    cabalPresent <- if allowCabals then return False else isCabalPresent
    if cabalPresent then return mempty else do
        psources <- getSources
        when (psources == mempty) $ terror $ "empty " <>| source_file

        let git_pkgs = gitPackages psources
        child_vendor_pkgs <- if null git_pkgs then return [] else do
          mkdir_p vendor_dir
          chdir vendor_dir $
            forM git_pkgs $ \pkg -> do
              let repo = gLocation pkg 
              let d = filename $ fromText repo
              e <- test_d d
              if not e
                then git_ "clone" ["--recursive", repo]
                else chdir d $ git_ "fetch" ["origin"]
              chdir d $ do
                git_ "checkout" [fromMaybe "master" (gTag pkg)]
                git_ "submodule" ["foreach", "git", "pull", "origin", "master"]
              readPackages False d

        child_dir_pkgs <- forM (dirs psources) $ \dir -> do
          b <- fmap (== fullDir) (path $ dLocation dir)
          if b then return mempty else readPackages False (dLocation dir)

        let child_pkgs = child_dir_pkgs ++ child_vendor_pkgs

        -- in the end we have either hackage packages or directories
        -- a directory was either listed as a directory or a child found in a sources.txt in that directory
        -- if there are no child, there will be an empty list [] of children
        -- this would be easy to break & should be cleaned up
        return $ mempty {
            hackages = hackages psources ++ concatMap hackages child_pkgs
          , dirs =
              concatMap (\(p, ps) -> if null ps then [p] else ps) $
                zip (dirs psources ++ gits psources ++ https psources) (map dirs child_pkgs)
          }
  where
    headMay [] = Nothing
    headMay xs = Just (head xs)

    isCabalFile = flip hasExtension "cabal"
    isCabalPresent = fmap (any isCabalFile) (ls ".")

    getSources :: ShIO PackageSources
    getSources = do
        sourceContent <- readfile source_file
        let sources = paritionSources [ source | 
              source <- map (T.words . T.strip) (T.lines sourceContent),
              not . null $ source,
              "--" /= head source
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
          go sources ((name:flags):more) = let c = T.head name in
            if c == '.' || c == '/'              then go sources { dirs = mkDir: dirs sources } more
              else if "http" `T.isPrefixOf` name then go sources { https = mkGit: https sources } more
              else if "git:" `T.isPrefixOf` name then go sources { gits = mkGit: gits sources } more
              else                                    go sources { hackages = mkPkg: hackages sources } more
            where
              mkDir = Directory (fromText name) flags
              mkPkg = Package name flags
              mkGit = let (realFlags, tags) = partition (T.isPrefixOf "-") flags in
                if length tags > 1
                  then error $ unpack $ "did not understand" <> T.intercalate " " (asList (Package name flags))
                  else GitPackage name realFlags (headMay tags)
