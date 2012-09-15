{-# LANGUAGE OverloadedStrings #-}
import Shelly
import CabalMeta
import Test.Hspec
import Data.Text.Lazy ()
import System.IO
import Filesystem.Path.CurrentOS hiding (fromText, (</>))

main :: IO ()
main = hspec $
  it "gets the packages" $ do
    (psources, wd) <- shelly $ verbosely $ do
      d <- pwd
      ps <- readPackages True "test"
      return (ps, d)

    let localize = (\p-> toTextIgnore $ wd </> fromText p)
    concatMap asList (packages psources) `shouldBe` [localize "test/child/grandchild", "top-package", "sphinx", "-fone-one-beta", "child-package"]
