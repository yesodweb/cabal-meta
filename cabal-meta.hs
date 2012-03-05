{-# LANGUAGE OverloadedStrings #-}
import CabalMeta
import Shelly
import System.Environment (getArgs)

import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy as LT
import Control.Monad (forM_)

main :: IO ()
main = do
  args <- fmap (map pack) getArgs
  shelly $ verbosely $ do
    packageSources <- readPackages True $ headDef "." args
    let pkgs = hackages packageSources ++ dirs packageSources
    let installs = map asList pkgs
    echo "Installing packages:"
    mapM_ echo $ map (LT.intercalate " ") installs
    _<- cabal_install $ concat installs
    forM_ (dirs packageSources) $ \pkg ->
      chdir (pLocation pkg) $ "cabal-src-install" # ["--src-only"]
    return ()

