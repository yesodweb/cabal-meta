{-# LANGUAGE OverloadedStrings #-}
import Shelly
import CabalMeta
import Test.Hspec.Monadic
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)
import Test.Hspec.HUnit ()
import Data.Text.Lazy ()
import System.IO
import Filesystem.Path.CurrentOS hiding (fromText, (</>))

main :: IO ()
main = hspecX $
  it "gets the packages" $ do
    (psources, wd) <- shelly $ verbosely $ do
      ps <- readPackages True "test"
      d <- pwd
      return (ps, d)

    let localize = (\p-> toTextIgnore $ wd </> fromText p)
    concatMap asList (packages psources) @?= [localize "test/child", localize "test/vendor/yesod/yesod", "sphinx", "-fone-one-beta", "fake-package"]
