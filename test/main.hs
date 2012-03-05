import Shelly
import CabalMeta

main :: IO ()
main = do
  shelly $ verbosely $ do
    pkgs <- readPackages True "test"
    inspect pkgs
