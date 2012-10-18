{-# LANGUAGE OverloadedStrings, CPP #-}
import CabalMeta
import OmniConfig
import Shelly
import Paths_cabal_meta

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Control.Monad (forM_)
import Data.Maybe (isNothing, isJust)
import Data.Text.Lazy (Text)
import Data.Version (showVersion)

import Filesystem.Path.CurrentOS (filename)
import Prelude hiding (FilePath)

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x

help :: Text
help = LT.intercalate "\n" [
  "cabal-meta is a cabal wrapper for installing multiple packages at once that may not be on hackage"
 ,"run with:"
 ,""
 ,"    cabal-meta [--[no-]dev] install [cabal install arguments]"
 ,""
 ,"       --dev means use cabal-dev instead of cabal"
 ,""
 ,"You can also set options through the CABAL_META_OPTS environment variable or the ~/.cabal-meta/opts file"
 ]

cabal_install_ :: CabalExe -> [Text] -> ShIO ()
cabal_install_ cabal = command_ (progName cabal) ["install"]

data CabalExe = Cabal | CabalDev deriving Show

progName :: CabalExe -> FilePath
progName Cabal = "cabal"
progName CabalDev = "cabal-dev"

assertCabalDependencies :: CabalExe -> IO ()
assertCabalDependencies Cabal    = shelly $ do
  mPath <- which "cabal-src-install"
  when (isNothing mPath) $
    errorExit "please run: cabal install cabal-src"

assertCabalDependencies CabalDev = do
  mcd <- shelly $ which "cabal-dev"
  case mcd of
    Just _ -> return ()
    Nothing -> error "--dev requires cabal-dev to be installed"

main :: IO ()
main = do
  allArgs <- fmap (filter $ not . T.null) $
    allProgramOpts [commandLine, environment "cabal-meta",
                    homeOptFile "cabal-meta"]

  when ("--version" `elem` allArgs) $ do
    putStrLn $ "cabal-meta " ++ showVersion version
    shelly $ exit 0

  let (mDev, noDevArgs) = checkNegatedOpt "dev" allArgs
  let isDev = isJust mDev

  let  cabal = if isDev then CabalDev else Cabal
  assertCabalDependencies cabal

  unless (headDef "" noDevArgs == "install") $ do
    putStrLn $ LT.unpack help
    putStrLn $ "using cabal: " ++ show cabal
    shelly $ exit 1

  let (_:args) = map LT.fromStrict noDevArgs

  shelly $ verbosely $ do
    packageSources <- readPackages True "."
    let installs = packageList packageSources
    echo "Installing packages:"
    mapM_ echo $ map (LT.intercalate " ") installs

    cabal_install_ cabal $ args ++ concat installs
    whenCabal cabal $ do
        forM_ (unstablePackages packageSources) $ \pkg ->
          chdir (diskPath pkg) $ run "cabal-src-install" ["--src-only"]
    return ()

  where
    whenCabal cabal a =
      case cabal of
        CabalDev -> return ()
        Cabal    -> a
