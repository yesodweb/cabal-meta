{-# LANGUAGE OverloadedStrings, CPP #-}
import CabalMeta
import Shelly
import System.Environment (getArgs)

import qualified Data.Text.Lazy as T
import Control.Monad (forM_, when, unless)
-- import Filesystem.Path.CurrentOS (decodeString)
import Data.Maybe (isNothing)
import Data.Text.Lazy (Text, pack)

import Filesystem.Path.CurrentOS (FilePath, filename)
import Prelude hiding (FilePath)

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x

help :: Text
help = T.intercalate "\n" [
  "cabal-meta is a cabal wrapper for packages not on hackage"
 ,"run with:"
 ,""
 ,"    cabal-meta [--dev] install [Cabal install arguments]"
 ,""
 ,"       --dev means use cabal-dev instead of cabal"
 ]

cabal_install_ :: CabalExe -> [Text] -> ShIO ()
cabal_install_ cabal = command_ (progName cabal) ["install"]

data CabalExe = Cabal | CabalDev

progName :: CabalExe -> FilePath
progName Cabal = "cabal"
progName CabalDev = "cabal-dev"

main :: IO ()
main = do
  cmdArgs <- fmap (map pack) getArgs
  shelly $ verbosely $ do
    let isDev = elem "--dev" cmdArgs
        noDevArgs = filter (/= "--dev") cmdArgs
        cabal = if isDev then CabalDev else Cabal

    unless (headDef "" noDevArgs == "install") $
      errorExit help

    let (_:args) = noDevArgs

    packageSources <- readPackages True "."
    ifCabal cabal $ do
      mPath <- which "cabal-src-install"
      when (isNothing mPath) $
        errorExit "please run: cabal install cabal-src"

    let installs = packageList packageSources
    echo "Installing packages:"
    mapM_ echo $ map (T.intercalate " ") installs

    cabal_install_ cabal $ args ++ concat installs
    ifCabal cabal $ do
        forM_ (dirs packageSources) $ \pkg ->
          chdir (dLocation pkg) $ run "cabal-src-install" ["--src-only"]
        forM_ (gitPackages packageSources) $ \pkg ->
          chdir vendor_dir $ do
            let repo = gLocation pkg 
            let d = filename $ fromText repo
            chdir d $ run "cabal-src-install" ["--src-only"]
    return ()

  where
    ifCabal cabal a =
      case cabal of
        CabalDev -> return ()
        Cabal    -> a
