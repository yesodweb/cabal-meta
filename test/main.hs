{-# LANGUAGE OverloadedStrings #-}
import Shelly
import CabalMeta
import Test.Hspec
import Data.Text.Lazy ()
import System.IO
import Filesystem.Path.CurrentOS hiding (fromText, (</>))
import Data.List (sort)

main :: IO ()
main = hspec $
  it "gets the packages" $ do
    (psources, wd) <- shelly $ verbosely $ do
      d <- pwd
      ps <- readPackages True "test"
      return (ps, d)

    let localize = (\p-> toTextIgnore $ wd </> fromText p)
    sort (concatMap asList (packages psources)) `shouldBe` sort [localize "test/child/grandchild", "top-package", "sphinx", "-fone-one-beta", "child-package"]
