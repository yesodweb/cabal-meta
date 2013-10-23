{-# LANGUAGE OverloadedStrings, CPP #-}
import CabalMeta
import OmniConfig
import Shelly
import Paths_cabal_meta

import qualified Data.Text as T
import Control.Monad (forM_)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Data.Version (showVersion)

import Prelude hiding (FilePath)

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x

help :: Text
help = T.intercalate "\n" [
  "cabal-meta is a cabal wrapper for installing multiple packages at once that may not be on hackage"
 ,"run with:"
 ,""
 ,"    cabal-meta [--[no-]dev] install [cabal install arguments]"
 ,""
 ,"       --dev means use cabal-dev instead of cabal"
 ,""
 ,"You can also set options through the CABAL_META_OPTS environment variable or the ~/.cabal-meta/opts file"
 ]

cabal_install_ :: CabalExe -> [Text] -> Sh ()
cabal_install_ cabal = command_ (progName cabal) ["install"]

data CabalExe = Cabal | CabalDev deriving Show

progName :: CabalExe -> FilePath
progName Cabal    = "cabal"
progName CabalDev = "cabal-dev"

assertCabalDependencies :: CabalExe -> IO Bool
assertCabalDependencies Cabal    = shelly $ do
    whenM (test_e "cabal-dev") $ do
      putStrLn $ T.unpack help
      echo "\n\ncabal-dev/ folder found. use the --dev option"
      quietExit 1

    mPath <- which "cabal-src-install"
    if isNothing mPath
      then warn >> return False
      else return True
  where
    warn = echo "\nWARNING: cabal-src not installed. run:\n   cabal install cabal-src\n"

assertCabalDependencies CabalDev = do
    mcd <- shelly $ which "cabal-dev"
    case mcd of
      Just _ -> return False
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

  unless (headDef "" noDevArgs == "install") $ do
    putStrLn $ T.unpack help
    putStrLn $ "using cabal: " ++ show cabal
    shelly $
      if (headDef "" noDevArgs == "--help") then exit 0 else quietExit 1

  installSrc <- assertCabalDependencies cabal

  let (_:args) = noDevArgs

  shelly $ verbosely $ do
    packageSources <- readPackages True "."
    let installs = packageList packageSources
    echo "Installing packages:"
    mapM_ echo $ map (T.intercalate " ") installs

    cabal_install_ cabal $ args ++ concat installs
    case (cabal, installSrc) of
      (Cabal, True) ->
        forM_ (unstablePackages packageSources) $ \pkg ->
          chdir (diskPath pkg) $ run "cabal-src-install" ["--src-only"]
      _ -> return ()
    return ()

  where
    whenCabal cabal a =
      case cabal of
        CabalDev -> return ()
        Cabal    -> a
