{-# LANGUAGE OverloadedStrings #-}
import CabalMeta
import Shelly
import System.Environment (getArgs)

import qualified Data.Text.Lazy as LT
import Control.Monad (forM_)
import Filesystem.Path.CurrentOS (decodeString)

main :: IO ()
main = do
  args <- fmap (map decodeString) getArgs
  shelly $ verbosely $ do
    packageSources <- readPackages True $ headDef "." args
    let pkgs = packages packageSources
    let installs = map asList pkgs
    echo "Installing packages:"
    mapM_ echo $ map (LT.intercalate " ") installs
    cabal_install_ $ concat installs
    forM_ (dirs packageSources) $ \pkg ->
      chdir (dLocation pkg) $ "cabal-src-install" # ["--src-only"]
    return ()

