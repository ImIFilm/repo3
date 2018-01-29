import qualified HUnitTests as HUT
import qualified QuickCheckTests as QCT
import System.Exit

main :: IO ()
main = do
  HUT.runTests
  good <- and <$> sequence [QCT.runTests]
  if good
     then exitSuccess
     else exitFailure
