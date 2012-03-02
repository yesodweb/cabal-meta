{-# LANGUAGE OverloadedStrings #-}

-- TODO:
-- * use --only-dependencies. otherwise a simple build failure will prevent a cabal-src-install
-- support git & tar.gz urls

import Shelly
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text.Lazy as LT
import Control.Monad (forM, forM_, when)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

source_file :: FilePath
source_file = "sources.txt"

cabal_install :: [Text] -> ShIO Text
cabal_install = command "cabal" ["install"]

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x

main :: IO ()
main = do
  args <- fmap (map pack) getArgs
  shelly $ verbosely $ do
    pkgs <- readPackages True $ headDef "." args
    echo "Installing packages:"
    forM_ pkgs echo
    _<-cabal_install pkgs
    forM_ pkgs $ \pkg ->
      chdir (unpack pkg) $ "cabal-src-install" # ["--src-only"]
    return ()

readPackages :: Bool -> Text -> ShIO [Text]
readPackages allowCabals dir = do
  fullDir <- path (unpack dir)
  chdir fullDir $ do
    cabalPresent <- if allowCabals then return False else isCabalPresent
    if cabalPresent then return [] else do
        pkgs <- getSources
        child_pkgs <- forM pkgs $ \pkg -> do
          b <- fmap (== fullDir) (path $ unpack pkg)
          if b  then return [] else readPackages False pkg
        return $ concatMap (\(p, ps) -> if null ps then [p] else ps) $ zip pkgs child_pkgs
  where
    isCabalFile f = ".cabal" == takeExtension f
    isCabalPresent = do
        cabals <- fmap (filter isCabalFile) $ ls "."
        return $ not $ null cabals 

    getSources :: ShIO [Text]
    getSources = do
        relative_pkgs <- fmap LT.lines $ readfile source_file
        when (null relative_pkgs) $ error $ "empty " ++ source_file
        mapM (liftAsString path) relative_pkgs
      where
        liftAsString f = fmap pack . f . unpack
