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
    pkgs <- readPackages True $ headDef "." args
    let installs = map asList pkgs
    echo "Installing packages:"
    mapM_ echo $ map (LT.intercalate " ") installs
    _<- cabal_install $ concat installs
    forM_ pkgs $ \pkg ->
      chdir (pLocation pkg) $ "cabal-src-install" # ["--src-only"]
    return ()

