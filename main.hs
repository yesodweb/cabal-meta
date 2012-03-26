{-# LANGUAGE OverloadedStrings, CPP #-}
import CabalMeta
import Shelly
import System.Environment (getArgs)

import qualified Data.Text.Lazy as T
import Control.Monad (forM_, unless)
import Filesystem.Path.CurrentOS (decodeString)
import Data.Maybe (isNothing)
import Data.Text.Lazy (Text)
import Control.Monad (when)


help :: Text
help = T.intercalate "\n" [
  "cabal-meta is a cabal wrapper for packages not on hackage"
 ,"run with:"
 ,""
 ,"    cabal-meta install"
 ]

main :: IO ()
main = do
  cmdArgs <- fmap (map decodeString) getArgs
  shelly $ verbosely $ do
    unless (headDef "" cmdArgs == "install") $
      errorExit help
    let (_:args) = cmdArgs

    packageSources <- readPackages True $ headDef "." args
    when (length (dirs packageSources) > 0) $ do
      mPath <- which "cabal-src-install"
      when (isNothing mPath) $
        errorExit "please run: cabal install cabal-src-install"

    let installs = packageList packageSources
    echo "Installing packages:"
    mapM_ echo $ map (T.intercalate " ") installs

    cabal_install_ $ concat installs
    forM_ (dirs packageSources) $ \pkg ->
      chdir (dLocation pkg) $ "cabal-src-install" # ["--src-only"]
    return ()

