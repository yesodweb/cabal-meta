{-# LANGUAGE CPP, OverloadedStrings #-}
module CabalMeta (
    Package (..)
  , UnstablePackage (..)
  , PackageSources (..)
  , readPackages
  , packageList
  , vendor_dir
  , unstablePackages
  , diskPath
#ifdef TEST
  , asList
  , packages
#endif
  ) where

import Shelly hiding (tag)
import Prelude hiding (FilePath)
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text.Lazy as T
import Filesystem.Path.CurrentOS (hasExtension, basename, dirname)
import Data.Maybe (fromMaybe, maybeToList, listToMaybe)
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

data Package = Unstable UnstablePackage
  | Package {
    pLocation :: Text
  , pFlags :: [Text]
} deriving (Show, Eq)

-- | An unstable package is one which has not been released to some
--   package repository
data UnstablePackage = Directory {
    dLocation :: FilePath
  , upFlags :: [Text]
} | GitPackage {
    gitLocation :: Text
  , upFlags :: [Text]
  , gTag :: Maybe Text
} | DarcsPackage {
    darcsLocation :: Text
  , upFlags :: [Text]
  , darcsTag :: Maybe Text
} deriving (Show, Eq)

asList :: Package -> [Text]
asList (Package l fs)                     = l : fs
asList (Unstable (GitPackage l fs tag))   = l : fs ++ maybeToList tag
asList (Unstable (DarcsPackage l fs tag)) = l : fs ++ maybeToList tag
asList (Unstable (Directory d fs))        = toTextIgnore d : fs

asInstallList :: Package -> [Text]
asInstallList p@(Package l _) = l     : flags p
asInstallList p@(Unstable up) = dpath : flags p
 where dpath = toTextIgnore (diskPath up)

flags :: Package -> [Text]
flags (Package _ fs)                   = fs
flags (Unstable (GitPackage _ fs _))   = fs
flags (Unstable (DarcsPackage _ fs _)) = fs
flags (Unstable (Directory _ fs))      = fs

diskPath :: UnstablePackage -> FilePath
diskPath p =
  case p of
   GitPackage l _ _   -> fromUrl l
   DarcsPackage l _ _ -> fromUrl l
   Directory  d _     -> d
 where
  fromUrl x = vendor_dir </> basename (fromText x)

data PackageSources = PackageSources {
    dirs     :: [UnstablePackage]
  , hackages :: [Package]
  , https    :: [UnstablePackage] -- also git for now
  , gits     :: [UnstablePackage]
  , darcsen  :: [UnstablePackage]
} deriving (Show, Eq)

packageList :: PackageSources -> [[Text]]
packageList = map asInstallList . packages

packages :: PackageSources -> [Package]
packages psources =
  hackages psources ++
  map Unstable (unstablePackages psources)

unstablePackages :: PackageSources -> [UnstablePackage]
unstablePackages psources =
  dirs psources ++
  gitPackages psources ++
  darcsen     psources

gitPackages :: PackageSources -> [UnstablePackage]
gitPackages psources =
  gits psources ++ https psources

instance Monoid PackageSources where
  mempty = PackageSources [] [] [] [] []
  mappend (PackageSources d1 ha1 ht1 g1 da1) (PackageSources d2 ha2 ht2 g2 da2) =
    PackageSources (mappend d1 d2) (mappend ha1 ha2)
      (mappend ht1 ht2) (mappend g1 g2) (mappend da1 da2)

vendor_dir :: FilePath
vendor_dir = "vendor"

git_ :: Text -> [Text] -> ShIO ()
git_ = command1_ "git" []

darcs_ :: Text -> [Text] -> ShIO ()
darcs_ = command1_ "darcs" []

readPackages :: Bool ->  FilePath -> ShIO PackageSources
readPackages allowCabals startDir = do
  fullDir <- canonic startDir
  chdir fullDir $ do
    cabalPresent <- if allowCabals then return False else isCabalPresent
    if cabalPresent then return mempty else do
        psources <- getSources
        when (psources == mempty) $ terror $ "empty " <> toTextIgnore source_file

        let remote_pkgs = gitPackages psources ++ darcsen psources
        unless (null remote_pkgs) $ mkdir_p vendor_dir
        child_vendor_pkgs <- forM remote_pkgs $ \pkg -> do
          updatePackage pkg
          kids <- readPackages False (diskPath pkg)
          return (pkg, kids)
        child_dir_pkgs <- forM (dirs psources) $ \dir -> do
          b <- fmap (== fullDir) (canonic $ dLocation dir)
          if b then return (dir, mempty)
               else do
                 kids <- readPackages False (dLocation dir)
                 return (dir, kids)

        let child_pkgs = child_dir_pkgs ++ child_vendor_pkgs

        -- in the end we have either hackage packages or directories
        -- a directory was either listed as a directory or a child found in a sources.txt in that directory
        -- if there are no child, there will be an empty list [] of children
        -- this would be easy to break & should be cleaned up
        return $ mempty {
            hackages = hackages psources ++ concatMap (hackages . snd) child_pkgs
          , dirs     = concatMap (\(p,ps) -> if null (dirs ps) then [p] else dirs ps) child_pkgs
          }
  where
    isCabalFile = flip hasExtension "cabal"
    isCabalPresent = fmap (any isCabalFile) (ls ".")
    updatePackage :: UnstablePackage -> ShIO ()
    updatePackage p@(GitPackage repo _ t) = do
      let d = diskPath p
      e <- test_d d
      if not e
        then chdir (dirname d) $
               git_ "clone" ["--recursive", repo]
        else chdir d $ git_ "fetch" ["origin"]
      chdir d $ do
        git_ "checkout" [fromMaybe "master" t]
        git_ "submodule" ["foreach", "git", "pull", "origin", "master"]
    updatePackage p@(DarcsPackage repo _ mtag) = do
      let d = diskPath p
          tflags = case mtag of
                     Nothing -> []
                     Just t  -> ["--tag", t]
      e <- test_d d
      if not e
        then chdir (dirname d) $
               darcs_ "get" $ ["--lazy", repo] ++ tflags
        else chdir d $ darcs_ "pull"  ["--all"]
    updatePackage (Directory _ _) = return mempty

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
          fp <- canonic $ dLocation package
          return package { dLocation = fp }

        paritionSources :: [[Text]] -> PackageSources
        paritionSources = go mempty
          where
          go sources [] = sources
          go _ ([]:_) = error "impossible"
          go sources ((name:flgs):more) = let n = T.head name in
            case () of
              _ | n `elem` "./"   -> next sources { dirs     = mkDir: dirs sources  }
                | prefix "http"   -> next sources { https    = mkGit: https sources }
                | prefix "https"  -> next sources { gits     = mkGit: https sources }
                | prefix "git:"   -> next sources { gits     = mkGit: gits sources  }
                | prefix "darcs:" -> next sources { darcsen  = mkDarcs: darcsen sources  }
                | otherwise       -> next sources { hackages = mkPkg: hackages sources }
            where
              prefix x = x `T.isPrefixOf` name
              next s2  = go s2 more
              mkDir = Directory (fromText name) flgs
              mkPkg = Package name flgs
              mkGit = GitPackage name realFlags tag
              mkDarcs =
                case T.stripPrefix "darcs:" name of
                  Nothing       -> error $ unpack $ "did not understand" <> T.intercalate " " (asList (Package name flgs))
                  Just realName -> DarcsPackage realName realFlags tag
              (realFlags, tag) = let (rf, tags) = partition (T.isPrefixOf "-") flgs in
                if length tags > 1
                  then error $ unpack $ "did not understand" <> T.intercalate " " (asList (Package name flgs))
                  else (rf, listToMaybe tags)
